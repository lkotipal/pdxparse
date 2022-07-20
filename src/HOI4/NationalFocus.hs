{-
Module      : HOI4.NationalFocus
Description : Feature handler for Hearts of Iron IV decisions
-}
module HOI4.NationalFocus (
        parseHOI4NationalFocuss
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

-- | Empty national focus. Starts off Nothing/empty everywhere, except id and name
-- (which should get filled in immediately).
newHOI4NationalFocus :: HOI4NationalFocus
newHOI4NationalFocus = HOI4NationalFocus "(Unknown)" "(Unknown)" Nothing Nothing "GFX_goal_unknown" undefined Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing undefined

-- | Take the decisions scripts from game data and parse them into decision
-- data structures.
parseHOI4NationalFocuss :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4NationalFocus)
parseHOI4NationalFocuss scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4NationalFocus $ concatMap mapTree scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing national focus: " ++ T.unpack err
            return HM.empty
        Right nfFilesOrErrors ->
            flip HM.traverseWithKey nfFilesOrErrors $ \sourceFile enfs ->
                fmap (mkNfMap . catMaybes) . forM enfs $ \case
                    Left err -> do
                        traceM $ "Error parsing national focus in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right nfocus -> return nfocus
    where
        mkNfMap :: [HOI4NationalFocus] -> HashMap Text HOI4NationalFocus
        mkNfMap = HM.fromList . map (nf_id &&& id)

        mapTree scr = case scr of
            [pdx| focus_tree = @focus |] -> focus
            [pdx| shared_focus = @_ |] -> [scr]
            _ -> []

-- | Parse a statement in an national focus file. Some statements aren't
-- national focus'; for those, and for any obvious errors, return Right Nothing.
parseHOI4NationalFocus :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4NationalFocus))
parseHOI4NationalFocus (StatementBare _) = throwError "bare statement at top level"
parseHOI4NationalFocus [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] ->
            if not (id == "focus" || id == "shared_focus") then
                return (Right Nothing)
            else
                withCurrentFile $ \file -> do
                    nfNameLoc <- getGameL10n $ fromMaybe (getNFId parts) (getNFTxt parts)
                    nfNameDesc <- getGameL10nIfPresent $ (fromMaybe (getNFId parts) (getNFTxt parts)) <> "_desc"
                    nnf <- hoistErrors $ foldM nationalFocusAddSection
                                                (Just newHOI4NationalFocus {nf_path = Just file
                                                                            ,nf_name_loc = nfNameLoc
                                                                            ,nf_name_desc = nfNameDesc})
                                                parts
                    case nnf of
                        Left err -> return (Left err)
                        Right Nothing -> return (Right Nothing)
                        Right (Just nf) -> withCurrentFile $ \file ->
                            return (Right (Just nf))
        _ -> throwError "unrecognized form for national focus (LHS)"
    _ -> return (Right Nothing)
    where
        getNFId ([pdx| id = $nfname|]:_) = nfname
        getNFId (_:xs) = getNFId xs
        getNFId [] = "(unknown)"
        getNFTxt ([pdx| txt = $nfname|]:_) = Just nfname
        getNFTxt (_:xs) = getNFTxt xs
        getNFTxt [] = Nothing
parseHOI4NationalFocus _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for national focus in " <> T.pack file)

-- | Interpret one section of an national focus. If understood, add it to the
-- event data. If not understood, throw an exception.
nationalFocusAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4NationalFocus -> GenericStatement -> PPT g m (Maybe HOI4NationalFocus)
nationalFocusAddSection Nothing _ = return Nothing
nationalFocusAddSection nf stmt
    = return $ (`nationalFocusAddSection'` stmt) <$> nf
    where
        nationalFocusAddSection' nf stmt@[pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "id" -> case rhs of
                GenericRhs txt [] -> nf { nf_id = txt}
                _-> trace ("bad nf id in: " ++ show stmt) nf
            "text" -> case rhs of
                GenericRhs txt [] -> nf { nf_text = Just txt}
                _-> trace ("bad nf id in: " ++ show stmt) nf
            "completion_reward" -> case rhs of
                CompoundRhs scr -> nf { nf_completion_reward = Just scr }
                _-> trace ("bad nf completion_reward") nf
            "icon" -> case rhs of
                GenericRhs txt [] ->
                    let txtd = if "GFX_" `T.isPrefixOf` txt then txt else "GFX_" <> txt in
                    nf { nf_icon = txtd}
                _-> trace ("bad nf icon in: " ++ show stmt) nf
            "cost" -> case rhs of
                (floatRhs -> Just num) -> nf {nf_cost = num}
                _ -> trace ("bad nf cost in: " ++ show stmt) nf
            "allow_branch" -> case rhs of
                _-> nf
            "x" -> case rhs of
                _-> nf
            "y" -> case rhs of
                _-> nf
            "prerequisite" -> case rhs of
                _-> nf
            "mutually_exclusive" -> case rhs of
                _-> nf
            "available" -> case rhs of
                _-> nf
            "bypass" -> case rhs of
                _-> nf
            "cancel" -> case rhs of
                _-> nf
            "cancelable" -> case rhs of --bool
                _-> nf
            "historical_ai" -> case rhs of
                _-> nf
            "available_if_capitulated" -> case rhs of --bool
                _-> nf
            "cancel_if_invalid" -> case rhs of --bool
                _-> nf
            "continue_if_invalid" -> case rhs of --bool
                _-> nf
            "will_lead_to_war_with" -> case rhs of
                _-> nf
            "search_filters" -> case rhs of
                _-> nf
            "select_effect" -> case rhs of
                CompoundRhs scr -> nf {nf_select_effect = Just scr}
                _-> trace ("bad nf select_effect in: " ++ show stmt) nf
            "ai_will_do" -> case rhs of --Do we want to deal with aistuff with focus' ?
                _-> nf
            "complete_tooltip" -> case rhs of
                _-> nf
            "offset" -> case rhs of
                _-> nf
            "relative_position_id" -> case rhs of
                _-> nf
            "dynamic" -> case rhs of
                _-> nf
            other -> trace ("unknown national focus section: " ++ show other ++ " for " ++ show stmt) nf
        nationalFocusAddSection' nf _
            = trace ("unrecognised form for national focus section") nf
