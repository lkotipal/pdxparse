{-
Module      : HOI4.Misc
Description : Feature handler for miscellaneous features in Hearts of Iron IV
-}
module HOI4.Misc (
         parseHOI4CountryHistory
        ,parseHOI4Interface
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Char (toLower)
import Data.Maybe (catMaybes, fromMaybe)
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
            = trace ("unrecognised form for set_politics in history") $ return cohi


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