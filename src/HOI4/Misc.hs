{-
Module      : HOI4.Misc
Description : Feature handler for miscellaneous features in Hearts of Iron IV
-}
module HOI4.Misc (
         parseHOI4CountryHistory
        ,parseHOI4Interface
        ,parseHOI4Characters
        ,parseHOI4CountryLeaderTraits
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid ((<>))

import System.FilePath (takeFileName, takeBaseName)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
import HOI4.Messages -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import HOI4.Common -- everything

newHOI4CountryHistory :: Text -> HOI4CountryHistory
newHOI4CountryHistory chtag = HOI4CountryHistory chtag undefined

parseHOI4CountryHistory :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4CountryHistory)
parseHOI4CountryHistory scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr -> setCurrentFile sourceFile $ mapM processPolitics $ concatMap mapHisto scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing country history: " ++ T.unpack err
            return HM.empty
        Right countryHistoryFilesOrErrors ->
            flip HM.traverseWithKey countryHistoryFilesOrErrors $ \sourceFile echist ->
                fmap (mkCHMap . catMaybes) . forM echist $ \case
                    Left err -> do
                        traceM $ "Error parsing country history in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right cchist -> return cchist
    where
        mkCHMap :: [HOI4CountryHistory] -> HashMap Text HOI4CountryHistory
        mkCHMap = HM.fromList . map (chTag &&& id)

        mapHisto scr = case scr of
            stmt@[pdx| set_politics = @pol |] -> [stmt]
            _ -> []

processPolitics :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4CountryHistory))
processPolitics (StatementBare _) = throwError "bare statement at top level"
processPolitics [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            let chtag = T.pack $ take 3 $ takeFileName file
            cchist <- hoistErrors $ foldM processPoliticsAddSection
                                        (Just (newHOI4CountryHistory chtag))
                                        parts
            case cchist of
                Left err -> return (Left err)
                Right Nothing -> return (Right Nothing)
                Right (Just chist) -> withCurrentFile $ \file ->
                    return (Right (Just chist ))
        _ -> throwError "unrecognized form for set_politics"
    _ -> throwError "unrecognized form for set_politics@content"
processPolitics _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for set_politics in " <> T.pack file)

processPoliticsAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4CountryHistory -> GenericStatement -> PPT g m (Maybe HOI4CountryHistory)
processPoliticsAddSection Nothing _ = return Nothing
processPoliticsAddSection cohi stmt
    = sequence (processPoliticsAddSection' <$> cohi <*> pure stmt)
    where
        processPoliticsAddSection' cohi stmt@[pdx| ruling_party = $id |] =
            let tag = chTag cohi in
            return cohi { chRulingTag = T.pack (concat[T.unpack tag , "_" , T.unpack id])}
        processPoliticsAddSection' cohi stmt@[pdx| last_election = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt@[pdx| election_frequency = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt@[pdx| elections_allowed = %_ |]
            = return cohi
        processPoliticsAddSection' cohi [pdx| $other = %_ |]
            = trace ("unknown set_politics in history: " ++ T.unpack other) $ return cohi
        processPoliticsAddSection' cohi _
            = trace "unrecognised form for set_politics in history" $ return cohi


parseHOI4Interface :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text Text)
parseHOI4Interface scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr -> setCurrentFile sourceFile $ mapM processInterface $ case scr of
                [[pdx| $spriteTypes = @spr |]] | T.toLower spriteTypes == "spritetypes" -> spr
                _ -> [])
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing interface: " ++ T.unpack err
            return HM.empty
        Right interfaceFilesOrErrors ->
            flip HM.traverseWithKey interfaceFilesOrErrors $ \sourceFile einterface ->
                fmap (mkInterMap . catMaybes) . forM einterface $ \case
                    Left err -> do
                        traceM $ "Error parsing interface in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right iinterface -> return iinterface
    where
        mkInterMap :: [(Text,Text)] -> HashMap Text Text
        mkInterMap interfacelist = HM.fromList interfacelist

processInterface :: (IsGameState (GameState g), MonadError Text m) => GenericStatement -> PPT g m (Either Text  (Maybe (Text, Text)))
processInterface stmt@[pdx| $spriteType = @spr |] | T.toLower spriteType == "spritetype"
    =  case (getId spr, getPic spr) of
        (Just id, Just pic) -> withCurrentFile $ \file -> return $ Right $ Just (id, pic)
        (Nothing, Just pic) -> return (Right Nothing)
        (Just id, Nothing) -> return (Right Nothing)
        _ -> return (Right Nothing)
    where
        getId :: [GenericStatement] -> Maybe Text
        getId [] = Nothing
        getId (stmt@[pdx| $name = $id |] : _)
            | T.toLower name == "name" = Just id
        getId (stmt@[pdx| $name = ?id |] : _)
            | T.toLower name == "name" = Just id
        getId (_ : ss) = getId ss
        getPic :: [GenericStatement] -> Maybe Text
        getPic [] = Nothing
        getPic (stmt@[pdx| $texturefile = $id |] : _)
            | T.toLower texturefile == "texturefile" = Just $ T.pack $ takeBaseName $ T.unpack id
        getPic (stmt@[pdx| $texturefile = ?id |] : _)
            | T.toLower texturefile == "texturefile" = Just $ T.pack $ takeBaseName $ T.unpack id
        getPic (_ : ss) = getPic ss
processInterface stmt = return (Right Nothing)

----------------
-- Characters --
----------------

newHOI4Character :: Text -> Text -> FilePath -> HOI4Character
newHOI4Character chatag locname = HOI4Character chatag locname Nothing Nothing Nothing Nothing

parseHOI4Characters :: (HOI4Info g, IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Character)
parseHOI4Characters scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr -> setCurrentFile sourceFile $ mapM character $ case scr of
                [[pdx| characters = @chars |]] -> chars
                _ -> scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing characters: " ++ T.unpack err
            return HM.empty
        Right characterFilesOrErrors ->
            flip HM.traverseWithKey characterFilesOrErrors $ \sourceFile ecchar ->
                fmap (mkChaMap . catMaybes) . forM ecchar $ \case
                    Left err -> do
                        traceM $ "Error parsing characters in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right cchar -> return cchar
    where
        mkChaMap :: [HOI4Character] -> HashMap Text HOI4Character
        mkChaMap = HM.fromList . map (chaTag &&& id)

character :: (HOI4Info g, IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Character))
character (StatementBare _) = throwError "bare statement at top level"
character [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            locname <- getGameL10n id
            cchar <- hoistErrors $ foldM characterAddSection
                                        (Just (newHOI4Character id locname file))
                                        parts
            case cchar of
                Left err -> return (Left err)
                Right Nothing -> return (Right Nothing)
                Right (Just char) -> withCurrentFile $ \file ->
                    return (Right (Just char))
        _ -> throwError "unrecognized form for characters"
    _ -> throwError "unrecognized form for characters@content"
character _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for characters in " <> T.pack file)

characterAddSection :: (HOI4Info g, MonadError Text m) =>
    Maybe HOI4Character -> GenericStatement -> PPT g m (Maybe HOI4Character)
characterAddSection Nothing _ = return Nothing
characterAddSection hChar stmt
    = sequence (characterAddSection' <$> hChar <*> pure stmt)
    where
        characterAddSection' hChar stmt@[pdx| name = %name |] = case name of
            StringRhs name -> return hChar {chaName = name}
            GenericRhs name [] -> do
                nameLoc <- getGameL10n name
                return hChar {chaName = nameLoc}
            _ -> trace "Bad name in characters" $ return hChar
        characterAddSection' hChar stmt@[pdx| advisor = @adv |] = do
            let  (onAdd, onRemove, advtraits) = getAdvinfo adv
            return hChar {chaAdvisorTraits = advtraits, chaOn_add = onAdd, chaOn_remove = onRemove}
        characterAddSection' hChar stmt@[pdx| country_leader = @clead |] =
            let cleadtraits = concatMap getTraits clead in
            return hChar {chaLeaderTraits = Just cleadtraits}
        characterAddSection' hChar [pdx| portraits = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| corps_commander = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| field_marshal = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| navy_leader = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| gender = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| instance = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| allowed_civil_war = %_ |] =
            return hChar
        characterAddSection' hChar [pdx| $other = %_ |]
            = trace ("unknown section in character: " ++ T.unpack other) $ return hChar
        characterAddSection' hChar _
            = trace "unrecognised form for in character" $ return hChar

        getTraits stmt@[pdx| traits = @traits |] = map
            (\case
                StatementBare (GenericLhs trait []) -> trait
                _ -> trace ("different trait list in" ++ show stmt) "")
            traits
        getTraits stmt = []

        getAdvinfo :: [GenericStatement] -> (Maybe GenericScript,Maybe GenericScript, Maybe [Text])
        getAdvinfo adv = do
            let (onadd, rest) = extractStmt (matchLhsText "on_add") adv
                (onremove, resttrait) = extractStmt (matchLhsText "on_remove") rest
                (traits,_ ) = extractStmt (matchLhsText "traits") rest
                onaddscr = case onadd of
                    Just [pdx| on_add = %rhs |] -> case rhs of
                        CompoundRhs [] -> Nothing
                        CompoundRhs scr -> Just scr
                        _-> Nothing
                    _ -> Nothing
                onremovescr = case onremove of
                    Just [pdx| on_remove = %rhs |] -> case rhs of
                        CompoundRhs [] -> Nothing
                        CompoundRhs scr -> Just scr
                        _-> Nothing
                    _ -> Nothing
                traitsl = case traits of
                    Just [pdx| traits = @rhs |] -> Just (mapMaybe traitFromArray rhs)
                    _-> Nothing
            (onaddscr, onremovescr, traitsl)
        traitFromArray :: GenericStatement -> Maybe Text
        traitFromArray (StatementBare (GenericLhs e [])) = Just e
        traitFromArray stmt = trace ("Unknown in character trait array: " ++ show stmt) Nothing

parseHOI4CountryLeaderTraits :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4CountryLeaderTrait)
parseHOI4CountryLeaderTraits scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4CountryLeaderTrait $ concatMap (\case
                [pdx| leader_traits = @traits |] -> traits
                _ -> [])
                scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing countryleadertraits: " ++ T.unpack err
            return HM.empty
        Right countryleadertraitsFilesOrErrors ->
            flip HM.traverseWithKey countryleadertraitsFilesOrErrors $ \sourceFile eclts ->
                fmap (mkCltMap . catMaybes) . forM eclts $ \case
                    Left err -> do
                        traceM $ "Error parsing countryleadertraits in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right cclt -> return cclt
                where mkCltMap :: [HOI4CountryLeaderTrait] -> HashMap Text HOI4CountryLeaderTrait
                      mkCltMap = HM.fromList . map (clt_id &&& id)

parseHOI4CountryLeaderTrait :: (IsGameData (GameData g), IsGameState (GameState g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4CountryLeaderTrait))
parseHOI4CountryLeaderTrait [pdx| $id = @effects |]
    = withCurrentFile $ \file -> do
        mlocid <- getGameL10nIfPresent id
        let cclt = foldl' addSection (HOI4CountryLeaderTrait {
                clt_id = id
            ,   clt_loc_name = mlocid
            ,   clt_path = file
            ,   clt_targeted_modifier = Nothing
            ,   clt_equipment_bonus = Nothing
            ,   clt_modifier = Nothing
            }) effects
        return $ Right (Just cclt)
    where
        addSection :: HOI4CountryLeaderTrait -> GenericStatement -> HOI4CountryLeaderTrait
        addSection clt stmt@[pdx| $lhs = @scr |] = case lhs of
            "targeted_modifier" -> clt { clt_targeted_modifier = Just stmt }
            "equipment_bonus" -> clt { clt_equipment_bonus = Just stmt }
            "ai_will_do" -> clt
            "random" -> clt
            _ -> trace ("Urecognized statement in dynamic modifier: " ++ show stmt) clt
         -- Must be an effect
        addSection clt stmt@[pdx| random = %_ |] = clt
        addSection clt stmt =
            let oldmod = fromMaybe [] (clt_modifier clt) in
            clt { clt_modifier = Just (oldmod ++ [stmt]) }
parseHOI4CountryLeaderTrait stmt = trace (show stmt) $ withCurrentFile $ \file ->
    throwError ("unrecognised form for dynamic modifier in " <> T.pack file)