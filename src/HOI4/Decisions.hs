{-
Module      : HOI4.Decisions
Description : Feature handler for Hearts of Iron IV decisions
-}
module HOI4.Decisions (
        parseHOI4Decisioncats,
        parseHOI4Decisions, writeHOI4Decisions
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

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
import Messages -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import HOI4.Common -- everything

-- | Empty decision category. Starts off Nothing/empty everywhere, except id and name
-- (which should get filled in immediately).
newDecisionCat id locid locdesc path = HOI4Decisioncat id locid locdesc Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing path

-- | Take the decisions categories scripts from game data and parse them into decision
-- data structures.
parseHOI4Decisioncats :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Decisioncat)
parseHOI4Decisioncats scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4Decisioncat scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing decision category: " ++ T.unpack err
            return HM.empty
        Right deccatFilesOrErrors ->
            flip HM.traverseWithKey deccatFilesOrErrors $ \sourceFile edeccats ->
                fmap (mkDecCatMap . catMaybes) . forM edeccats $ \case
                    Left err -> do
                        traceM $ "Error parsing decision categories in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right ddeccat -> return ddeccat
                where mkDecCatMap :: [HOI4Decisioncat] -> HashMap Text HOI4Decisioncat
                      mkDecCatMap = HM.fromList . map (decc_name &&& id)

-- | Parse one decisioncategory script into a decision data structure.
parseHOI4Decisioncat :: (IsGameData (GameData g), IsGameState (GameState g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4Decisioncat))
parseHOI4Decisioncat (StatementBare _) = throwError "bare statement at top level"
parseHOI4Decisioncat [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            locid <- getGameL10nIfPresent id
            locdesc <- getGameL10nIfPresent (id <> "_desc")
            ddeccat <- hoistErrors $ foldM decisioncatAddSection
                                        (Just (newDecisionCat id locid locdesc file))
                                        parts
            case ddeccat of
                Left err -> return (Left err)
                Right Nothing -> return (Right Nothing)
                Right (Just deccat) -> withCurrentFile $ \file ->
                    return (Right (Just deccat ))
        _ -> throwError "unrecognized form for decision category"
    _ -> throwError "unrecognized form for decision category"
parseHOI4Decisioncat _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for decisi= reon category in " <> T.pack file)

-- | Add a sub-clause of the decision categry script to the data structure.
decisioncatAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4Decisioncat -> GenericStatement -> PPT g m (Maybe HOI4Decisioncat)
decisioncatAddSection Nothing _ = return Nothing
decisioncatAddSection ddeccat stmt
    = return $ (`decisioncatAddSection'` stmt) <$> ddeccat
    where
        decisioncatAddSection' decc stmt = case stmt of
            [pdx| icon           = $txt  |] -> (decc { decc_icon = Just txt })
            [pdx| visible        = @scr  |] -> (decc { decc_visible = Just scr })
            [pdx| available      = @scr  |] -> (decc { decc_available = Just scr })
            [pdx| picture        = $txt  |] -> (decc { decc_picture = Just txt })
            [pdx| custom_icon    = %_    |] -> decc
            [pdx| visibility_type = %_   |] -> decc
            [pdx| priority       = %_    |] -> decc
            [pdx| allowed        = @scr  |] -> (decc { decc_allowed = Just scr })
            [pdx| visible_when_empty = %_ |] -> decc
            [pdx| scripted_gui   = %_    |] -> decc
            [pdx| highlight_states = %_  |] -> decc
            [pdx| on_map_area    = %_    |] -> decc
            [pdx| $other = %_ |] -> trace ("unknown decision category section: " ++ T.unpack other) $ decc
            _ -> trace ("unrecognised form for decision category section") $ decc

-- | Empty decision. Starts off Nothing/empty everywhere, except id and name
-- (which should get filled in immediately).
newDecision :: HOI4Decision
newDecision = HOI4Decision undefined undefined Nothing Nothing Nothing Nothing Nothing Nothing Nothing False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False Nothing False False False False Nothing Nothing Nothing undefined

-- | Take the decisions scripts from game data and parse them into decision
-- data structures.
parseHOI4Decisions :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4Decision)
parseHOI4Decisions scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ concat <$> mapM parseHOI4DecisionGroup scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing decisions: " ++ T.unpack err
            return HM.empty
        Right decFilesOrErrors ->
            flip HM.traverseWithKey decFilesOrErrors $ \sourceFile edecs ->
                fmap (mkDecMap . catMaybes) . forM edecs $ \case
                    Left err -> do
                        traceM $ "Error parsing decisions in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right ddec -> return ddec
                where mkDecMap :: [HOI4Decision] -> HashMap Text HOI4Decision
                      mkDecMap = HM.fromList . map (dec_name &&& id)

--parseHOI4Event :: MonadError Text m => FilePath -> GenericStatement -> PPT g m (Either Text (Maybe HOI4Event))

-- | Parse one file's decision groups scripts into decision data structures.
parseHOI4DecisionGroup :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    GenericStatement -> PPT g (ExceptT Text m) [Either Text (Maybe HOI4Decision)]
parseHOI4DecisionGroup stmt@(StatementBare _) = trace ("DEBUG: BARE: " ++ show stmt) $ return [(Right Nothing)]
parseHOI4DecisionGroup [pdx| $left = @scr |]
    = forM scr $ \stmt -> (Right <$> parseHOI4Decision stmt left)
                            `catchError` (return . Left)
parseHOI4DecisionGroup [pdx| %check = %_ |] = case check of
    AtLhs _ -> return [(Right Nothing)]
    _-> throwError "unrecognized form for decision block (LHS)"
parseHOI4DecisionGroup _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for decision in " <> T.pack file)

-- | Parse one decision script into a decision data structure.
parseHOI4Decision :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    GenericStatement -> Text -> PPT g (ExceptT Text m) (Maybe HOI4Decision)
parseHOI4Decision [pdx| $decName = %rhs |] category = case rhs of
    CompoundRhs parts -> do
        decName_loc <- getGameL10n decName
        decText <- getGameL10nIfPresent (decName <> "_desc")
        withCurrentFile $ \sourcePath ->
            foldM decisionAddSection
                  (Just (newDecision { dec_name = decName
                              , dec_name_loc = decName_loc
                              , dec_path = Just (sourcePath </> T.unpack category) -- so decision are divided into maps for the cateogry, should I loc or not?
                              , dec_cat = category}))
                  parts
    _ -> throwError "unrecognized form for decision (RHS)"
parseHOI4Decision [pdx| %check = %_ |] _ = case check of
    AtLhs _ -> return Nothing
    _-> throwError "unrecognized form for decision block (LHS)"
parseHOI4Decision _ _ = throwError "unrecognized form for decision (LHS)"

-- | Add a sub-clause of the decision script to the data structure.
decisionAddSection :: (IsGameData (GameData g),  MonadError Text m) =>
    Maybe HOI4Decision -> GenericStatement -> PPT g m (Maybe HOI4Decision)
decisionAddSection Nothing _ = return Nothing
decisionAddSection dec stmt
    = return $ (`decisionAddSection'` stmt) <$> dec
    where -- the QQ pdx patternmatching takes to long to compile with this many patterns so using case of here
        decisionAddSection' dec stmt@[pdx| $lhs = %rhs |] = case lhs of
            "icon" -> case rhs of
                GenericRhs txt _ -> dec { dec_icon = Just (HOI4DecisionIconSimple txt) }
                CompoundRhs scr -> dec { dec_icon = Just (HOI4DecisionIconScript scr) }
                _ -> trace ("DEBUG: bad decions icon") $ dec
            "allowed" -> case rhs of
                CompoundRhs scr -> dec { dec_allowed = Just scr }
                _ -> dec
            "complete_effect" -> case rhs of
                CompoundRhs scr -> dec { dec_complete_effect = Just scr }
                _ -> dec
            "ai_will_do" -> case rhs of
                CompoundRhs scr -> dec { dec_ai_will_do = Just (aiWillDo scr) }
                _ -> dec
            "target_root_trigger" -> case rhs of
                _ -> dec
            "visible" -> case rhs of
                _ -> dec
            "available" -> case rhs of
                _ -> dec
            "priority" -> case rhs of
                _ -> dec
            "highlight_states" -> case rhs of
                _ -> dec
            "days_re_enable" -> case rhs of
                _ -> dec
            "fire_only_once"  -> case rhs of --bool, standard false
                _ -> dec
            "cost" -> case rhs of --var or num
                _ -> dec
            "custom_cost_trigger" -> case rhs of
                _ -> dec
            "custom_cost_text" -> case rhs of
                _ -> dec
            "days_remove" -> case rhs of
                _ -> dec
            "remove_effect" -> case rhs of
                CompoundRhs scr -> dec { dec_remove_effect = Just scr }
                _ -> dec
            "cancel_trigger" -> case rhs of
                _ -> dec
            "cancel_effect" -> case rhs of
                CompoundRhs scr -> dec { dec_cancel_effect = Just scr }
                _ -> dec
            "war_with_on_remove" -> case rhs of
                _ -> dec
            "war_with_on_complete" -> case rhs of
                _ -> dec
            "fixed_random_seed" -> case rhs of --bool, standard True
                _ -> dec
            "days_mission_timeout" -> case rhs of
                _ -> dec
            "activation" -> case rhs of
                _ -> dec
            "selectable_mission" -> case rhs of --bool, standard false
                _ -> dec
            "timeout_effect" -> case rhs of
                CompoundRhs scr -> dec { dec_timeout_effect = Just scr }
                _ -> dec
            "is_good" -> case rhs of --bool, standard false but not really
                _ -> dec
            "targets" -> case rhs of -- weirdo array
                _ -> dec
            "target_array" -> case rhs of -- weirdo array
                _ -> dec
            "targets_dynamic" -> case rhs of --bool, standard false
                _ -> dec
            "target_trigger" -> case rhs of
                _ -> dec
            "war_with_target_on_complete" -> case rhs of --bool, standard false
                _ -> dec
            "war_with_target_on_remove" -> case rhs of --bool, standard false
                _ -> dec
            "war_with_target_on_timeout" -> case rhs of --bool, standard false
                _ -> dec
            "state_target" -> case rhs of --bool, standard false
                _ -> dec
            "on_map_mode" -> case rhs of
                _ -> dec
            "modifier" -> case rhs of
                _ -> dec
            "targeted_modifier" -> case rhs of
                _ -> dec
            "cancel_if_not_visible" -> case rhs of
                _ -> dec
            "name" -> case rhs of
                _ -> dec
            "ai_hint_pp_cost" -> case rhs of
                _ -> dec
            "cosmetic_tag" -> case rhs of
                _ -> dec
            "cosmetic_ideology" -> case rhs of
                _ -> dec
            "remove_trigger" -> case rhs of
                _ -> dec
            "target_non_existing" -> case rhs of
                _ -> dec
            other -> trace ("unknown decision section: " ++ show other ++ "  " ++ show stmt) $ dec
        decisionAddSection' dec stmt = trace ("unrecognised form for decision section") $ dec

-- | Present the parsed decisions as wiki text and write them to the
-- appropriate files.
writeHOI4Decisions :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Decisions = do
    decisions <- getDecisions
    let pathedDecisions :: [Feature HOI4Decision]
        pathedDecisions = map (\dec -> Feature {
                                        featurePath = dec_path dec
                                    ,   featureId = Just (dec_name dec) <> Just ".txt"
                                    ,   theFeature = Right dec })
                              (HM.elems decisions)
    writeFeatures "decisions"
                  pathedDecisions
                  pp_decision

-- | Present a parsed decision.
pp_decision :: (HOI4Info g, Monad m) => HOI4Decision -> PPT g m Doc
pp_decision dec = do
    version <- gets (gameVersion . getSettings)
    dec_text_loc <- getGameL10nIfPresent ((dec_name dec) <> "_desc")
--    allow_pp'd  <- scope HOI4Country (pp_script (dec_allowed dec))
--    effect_pp'd <- setIsInEffect True (scope HOI4Country (pp_script (dec_complete_effect dec)))
    mawd_pp'd   <- setIsInEffect True (scope HOI4Country (mapM ((imsg2doc =<<) . ppAiWillDo) (dec_ai_will_do dec)))
    let name = dec_name dec
        nameD = Doc.strictText name
    name_loc <- getGameL10n name
    return . mconcat $
        ["<section begin=", nameD, "/>"
        ,"{{Decision", PP.line
        ,"| version = ", Doc.strictText version, PP.line
        ,"| decision_id = ", nameD, PP.line
        ,"| decision_name = ", Doc.strictText name_loc, PP.line
        ,maybe mempty
               (\txt -> mconcat ["| decision_text = ", Doc.strictText txt, PP.line])
               (dec_text_loc)
        ] ++
--        (maybe [] (\app -> ["| allow = ", PP.line, app, PP.line]) allow_pp'd) ++
--        (maybe [] (\epp -> ["| effect = ", PP.line, epp, PP.line]) effect_pp'd) ++
        flip (maybe []) mawd_pp'd (\awd_pp'd ->
            ["| comment = AI decision factors:", PP.line
            ,awd_pp'd, PP.line]) ++
        ["}}" -- no line, causes unwanted extra space
        ,"<section end=", nameD, "/>"
        ]
