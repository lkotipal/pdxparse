{-|
Module      : HOI4.Ideas
Description : Feature handler for Hearts of Iron IV idea groups
-}
module HOI4.Ideas (
        HOI4Idea (..)
    ,   parseHOI4Ideas
--    ,   writeHOI4Ideas
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Char (toLower)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>))
import Data.List (intersperse, foldl', intercalate)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import System.FilePath ((</>))

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
import HOI4.Messages -- everything
import MessageTools (iquotes)
import HOI4.Handlers (flagText, getStateLoc)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import HOI4.Common -- everything

-- | Take the idea group scripts from game data and parse them into idea group
-- data structures.
parseHOI4Ideas :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Idea)
parseHOI4Ideas scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ concat <$> mapM parseHOI4IdeaGroup (case scr of
                    [[pdx| ideas = @mods |]] -> mods
                    _ -> scr))
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing ideas: " ++ T.unpack err
            return HM.empty
        Right ideasFilesOrErrors ->
            flip HM.traverseWithKey ideasFilesOrErrors $ \sourceFile eidea ->
                fmap (mkIdeaMap . catMaybes) . forM eidea $ \case
                    Left err -> do
                        traceM $ "Error parsing ideas in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right iidea -> return iidea
                where mkIdeaMap :: [HOI4Idea] -> HashMap Text HOI4Idea
                      mkIdeaMap = HM.fromList . map (id_id &&& id)

-- | Parse one file's idea groups scripts into idea data structures.
parseHOI4IdeaGroup :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    GenericStatement -> PPT g (ExceptT Text m) [Either Text (Maybe HOI4Idea)]
parseHOI4IdeaGroup stmt@(StatementBare _) = throwError "bare statement at top level"
parseHOI4IdeaGroup [pdx| $left = @scr |]
    = forM scr $ \stmt -> (Right <$> parseHOI4Idea stmt left)
                            `catchError` (return . Left)
parseHOI4IdeaGroup [pdx| %check = %_ |] = case check of
    AtLhs _ -> return [(Right Nothing)]
    _-> throwError "unrecognized form for idea block (LHS)"
parseHOI4IdeaGroup _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for ideas in " <> T.pack file)

-- | Empty idea. Starts off Nothing everywhere, except id and name
-- (should get filled in immediately).
newIdea :: HOI4Idea
newIdea = HOI4Idea undefined undefined Nothing Nothing "GFX_idea_unknown" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing undefined undefined

-- | Parse one idea script into a idea data structure.
parseHOI4Idea :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    GenericStatement -> Text -> PPT g (ExceptT Text m) (Maybe HOI4Idea)
parseHOI4Idea [pdx| $ideaName = %rhs |] category = case rhs of
    CompoundRhs parts -> do
        idName_loc <- getGameL10n ideaName
        let idPicture = "GFX_idea_" <> ideaName
        idDesc <- getGameL10nIfPresent (ideaName <> "_desc")
        withCurrentFile $ \sourcePath ->
            foldM ideaAddSection
                  (Just (newIdea { id_id = ideaName
                              , id_name = ideaName
                              , id_name_loc = Just idName_loc
                              , id_desc_loc = idDesc
                              , id_picture = idPicture
                              , id_path = sourcePath </> T.unpack category -- so ideas are divided into maps for the cateogry, should I loc or not?
                              , id_category = category}))
                  parts
    GenericRhs txt [] -> if ideaName == "designer" || ideaName == "use_list_view" || ideaName == "law" then return Nothing else throwError "unrecognized form for idea (RHS) "
    _ -> throwError "unrecognized form for idea (RHS)"
parseHOI4Idea [pdx| %check = %_ |] _ = case check of
    AtLhs _ -> return Nothing
    _-> throwError "unrecognized form for idea block (LHS)"
parseHOI4Idea _ _ = throwError "unrecognized form for idea (LHS)"

-- | Interpret one section of an opinion modifier. If understood, add it to the
-- event data. If not understood, throw an exception.
ideaAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4Idea -> GenericStatement -> PPT g m (Maybe HOI4Idea)
ideaAddSection Nothing _ = return Nothing
ideaAddSection iidea stmt
    = return $ (`ideaAddSection'` stmt) <$> iidea
    where
        ideaAddSection' iidea stmt@[pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "picture"   -> case rhs of
                GenericRhs txt [] ->
                    let txtd = if "GFX_idea_" `T.isPrefixOf` txt then txt else "GFX_idea_" <> txt in
                    iidea { id_picture = txtd }
                _-> trace ("bad idea picture") iidea
            "name"      -> case rhs of
                GenericRhs txt [] ->  iidea { id_name = txt }
                _-> trace ("bad idea name") iidea
            "modifier"  -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_modifier = Just scr }
                _-> trace ("bad idea modifer") iidea
            "targeted_modifier" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_targeted_modifier = Just scr }
                _-> trace ("bad idea targeted_modifier") iidea
            "research_bonus" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_research_bonus = Just scr }
                _-> trace ("bad idea reearch_bonus") iidea
            "equipment_bonus" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_equipment_bonus = Just scr }
                _-> trace ("bad idea equipment_bonus") iidea
            "allowed" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_allowed = Just scr }
                _-> trace ("bad idea allowed") iidea
            "visible" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_visible = Just scr }
                _-> trace ("bad idea visible") iidea
            "available" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_available = Just scr }
                _-> trace ("bad idea available") iidea
            "on_add" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_on_add = Just scr }
                _-> trace ("bad idea on_add") iidea
            "on_remove" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_on_remove = Just scr }
                _-> trace ("bad idea on_remove") iidea
            "cancel" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_cancel = Just scr }
                _-> trace ("bad idea cancel") iidea
            "do_effect" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_do_effect = Just scr }
                _-> trace ("bad idea do_effect") iidea
            "rule" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_rule = Just scr }
                _-> trace ("bad idea rule") iidea
            "allowed_civil_war" -> case rhs of
                CompoundRhs [] -> iidea
                CompoundRhs scr -> iidea { id_allowed_civil_war = Just scr }
                _-> trace ("bad idea allowed_civil_war") iidea
            "ai_will_do"        -> iidea
            "cancel_if_invalid" -> iidea
            "removal_cost"      -> iidea
            "level"             -> iidea
            "allowed_to_remove" -> iidea
            "cost"              -> iidea
            "traits"            -> iidea
            "ledger"            -> iidea
            "default"           -> iidea
            other               -> trace ("unknown idea section: " ++ T.unpack other) $ iidea
        ideaAddSection' iidea _
            = trace ("unrecognised form for idea section") iidea
{-
-- | Present the parsed ideas as wiki text and write them to the
-- appropriate files.
writeHOI4Ideas :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Ideas = do
    ideas <- getIdeas
    let pathedIdeas :: [Feature HOI4Idea]
        pathedIdeas = map (\id -> Feature {
                                    featurePath = id_path id
                                ,   featureId = Just (id_name id)
                                ,   theFeature = Right id })
                            (HM.elems ideas)
    writeFeatures "ideas"
                  pathedIdeas
                  (\d -> scope HOI4Country $ pp_idea d interface)

-- | Present a single idea
pp_idea :: (HOI4Info g, Monad m) => HOI4Idea -> PPT g (ExceptT Text m) Doc
pp_idea ig = fixup <$> do
    version <- gets (gameVersion . getSettings)
    let name = ig_name_loc ig
    case (ig_bonus ig, length (ig_ideas ig)) of
        (Just bonus, 7) -> do
            let rawideas = ig_ideas ig
                unindent = map (first (const 0))
            ideas <- forM rawideas $ \idea -> do
                effmsgs <- ppMany (idea_effects idea)
                case effmsgs of
                    -- Remove the bullets from a single effect
                    [_] -> imsg2doc (unindent effmsgs)
                    {- This doesn't work, due to the template's abuse of
                       deflist markup and misbehaviour of MediaWiki.
                    -- Wrap multiple effects in a plainlist
                    _ -> do
                        effsdoc <- imsg2doc effmsgs
                        return $ templateDoc "plainlist" [effsdoc]
                    -}
                    -- Instead, replace bullets with colons.
                    [] -> return mempty
                    (msg:msgs) -> do
                        firstmsg <- imsg2doc (unindent [msg])
                        rest <- mapM (\m -> (":" <>) <$> imsg2doc [m]) (unindent msgs)
                        return (firstmsg <> PP.line <> PP.vsep rest)
            bonus_pp'd <- imsg2doc . unindent =<< ppMany bonus
            mtrigger_pp'd <- case ig_trigger ig of
                Nothing -> return Nothing
                Just trigger -> Just <$> (imsg2doc =<< ppMany trigger)
            let name_loc = Doc.strictText . T.replace " Ideas" "" $ name
                ig_id_t = ig_name ig
                ig_id = Doc.strictText ig_id_t
            trads <- case ig_start ig of
                Just [trad1s, trad2s] -> do
                    trad1 <- imsg2doc . map (first (const 0)) =<< ppOne trad1s
                    trad2 <- imsg2doc . map (first (const 0)) =<< ppOne trad2s
                    return $ Right (trad1, trad2)
                Just trads -> return . Left . Just . length $ trads
                Nothing -> return (Left Nothing)
            return . mconcat $
                ["<section begin=", name_loc, "/>", PP.line
                ,"{{idea group", PP.line
                ,"|version=", Doc.strictText version, PP.line
                ,"|name2=", name_loc, PP.line
                ,"|name=", name_loc, PP.line
                ,case ig_category ig of
                    Nothing -> case trads of
                        Right (trad1, trad2) -> mconcat -- assume groups with no category are country ideas
                            ["|tradition1=", trad1, PP.line
                            ,"|tradition2=", trad2, PP.line
                            ]
                        Left (Just ntrads) -> mconcat
                            ["<!-- Looks like a country idea group, but has non-standard number of traditions ("
                            ,Doc.strictText (T.pack (show ntrads))
                            ,") -->", PP.line]
                        Left Nothing -> mconcat
                            ["<!-- Looks like a country idea group, but has no traditions -->", PP.line]
                    Just cat -> mconcat
                        ["<!-- Category: ", Doc.pp_string (show cat), " -->", PP.line
                        ,"| events = ", name_loc, " idea group events", PP.line]
                ,"|idea1=", Doc.strictText (idea_name_loc (rawideas !! 0)), PP.line
--                 ,iconForIdea (rawideas !! 0)
                ,"|idea1effect=", ideas !! 0, PP.line
                ,"|idea2=", Doc.strictText (idea_name_loc (rawideas !! 1)), PP.line
--                 ,iconForIdea (rawideas !! 1)
                ,"|idea2effect=", ideas !! 1, PP.line
                ,"|idea3=", Doc.strictText (idea_name_loc (rawideas !! 2)), PP.line
--                 ,iconForIdea (rawideas !! 2)
                ,"|idea3effect=", ideas !! 2, PP.line
                ,"|idea4=", Doc.strictText (idea_name_loc (rawideas !! 3)), PP.line
--                 ,iconForIdea (rawideas !! 3)
                ,"|idea4effect=", ideas !! 3, PP.line
                ,"|idea5=", Doc.strictText (idea_name_loc (rawideas !! 4)), PP.line
--                 ,iconForIdea (rawideas !! 4)
                ,"|idea5effect=", ideas !! 4, PP.line
                ,"|idea6=", Doc.strictText (idea_name_loc (rawideas !! 5)), PP.line
--                 ,iconForIdea (rawideas !! 5)
                ,"|idea6effect=", ideas !! 5, PP.line
                ,"|idea7=", Doc.strictText (idea_name_loc (rawideas !! 6)), PP.line
--                 ,iconForIdea (rawideas !! 6)
                ,"|idea7effect=", ideas !! 6, PP.line
                ,"|bonus=", bonus_pp'd, PP.line
                ] ++ (case mtrigger_pp'd of
                    Just trigger_pp'd ->
                        ["| notes = Can be selected only if the following are true:", PP.line
                        ,trigger_pp'd
                        ,PP.line]
                    Nothing -> [])
                ++ ["}}", PP.line
                ,"<section end=", name_loc, "/>"]
        (Nothing, _) -> throwError $ "Idea group " <> name <> " has no bonus"
        (_, n) -> throwError $ "Idea group " <> name <> " has non-standard number of ideas (" <> T.pack (show n) <> ")"
-}