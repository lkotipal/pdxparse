{-|
Module      : HOI4.Missions
Description : Feature handler for Europa Universalis IV missions
-}
module HOI4.Missions (
        parseHOI4Missions, writeHOI4Missions
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (liftM, forM, forM_, foldM, when, (<=<))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.List (intersperse, find, foldl')
import Data.Maybe (isJust, isNothing, fromMaybe, fromJust, catMaybes)
import Data.Monoid ((<>))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import HOI4.Common -- everything
import FileIO (Feature (..), writeFeatures)
import Messages (imsg2doc)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)

-- TODO: Maybe this should be a setting instead?
-- useIconBox:
--   True -> Format as french missions
--   False -> Format as generic missions
useIconBox = True

newMission :: Text -> Int -> Int -> HOI4Mission
newMission id slot pos = HOI4Mission id "(unknown)" slot pos [] undefined undefined

newMissionBranch :: FilePath -> Text -> HOI4MissionTreeBranch
newMissionBranch path id = HOI4MissionTreeBranch path id 0 Nothing []

parseHOI4Missions :: (IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4MissionTreeBranch)
parseHOI4Missions scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4MissionTree scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing missions: " ++ T.unpack err
            return HM.empty
        Right missionsFilesOrErrors ->
            flip HM.traverseWithKey missionsFilesOrErrors $ \sourceFile emissions -> do
                fmap (mkMisMap . catMaybes) . forM emissions $ \case
                    Left err -> do
                        traceM $ "Error parsing missions in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right mevt -> return mevt
                where mkMisMap :: [HOI4MissionTreeBranch] -> HashMap Text HOI4MissionTreeBranch
                      mkMisMap = HM.fromListWith combineBranches . map (hoi4mtb_id &&& id)
                      -- In 1.31.3 mnd_plb_1 appears twice in MND_Palembang_Missions.txt
                      -- Assume it just adds new missions and doesn't change requirements
                      -- Note: Parts of the mission tree will disappear after re-loading a saved game, so issue a warning.
                      combineBranches :: HOI4MissionTreeBranch -> HOI4MissionTreeBranch -> HOI4MissionTreeBranch
                      combineBranches new old = (trace $ "Warning: Merging mission tree branches for " ++ T.unpack (hoi4mtb_id old) ++ ". Some missions will disappear after re-loading a save.") $
                        old {hoi4mtb_missions = (hoi4mtb_missions old) ++ (hoi4mtb_missions new)}

parseHOI4MissionTree :: (IsGameState (GameState g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4MissionTreeBranch))
parseHOI4MissionTree [pdx| $serid = @scr |] = withCurrentFile $ \file -> do
    hoistErrors (foldM handleBranch (Just (newMissionBranch file serid)) scr)
    where
        handleBranch :: (IsGameState (GameState g), MonadError Text m) =>
            Maybe HOI4MissionTreeBranch -> GenericStatement -> PPT g m (Maybe HOI4MissionTreeBranch)
        handleBranch (Just mtb) [pdx| slot = !rhs |] = do -- <int> Which column the missions will appear in. 1 to 5.
            return $ Just $ mtb {hoi4mtb_slot = rhs }
        handleBranch mtb [pdx| generic = %rhs |] = return mtb -- <bool> Whether the AI will claim missions in this series.
        handleBranch mtb [pdx| ai = %rhs |] = return mtb -- <bool> Whether the AI will claim missions in this series.
        handleBranch mtb [pdx| has_country_shield = %rhs |] = return mtb -- <bool> Whether to display the country shield on the icon.
        handleBranch mtb [pdx| potential_on_load = @scr |] = return mtb -- <compound> Determines whether a series is loaded at all. Used to limit series to DLC.
        handleBranch (Just mtb) [pdx| potential = @scr |] = do -- <compound> Determines when a series appears for a country. Country scope.
            return $ Just $ mtb {hoi4mtb_potential = Just scr }
        handleBranch (Just mtb) [pdx| $id = @scr |] = do
            let ms = hoi4mtb_missions mtb
            -- Guess initial position (can also be expliticly set by the mission)
            let pos = 1 + (if null ms then 0
                       else hoi4m_position (last ms))
            mission <- foldM handleMission (newMission id (hoi4mtb_slot mtb) pos) scr
            -- TODO: Ensure trigger and effect are present
            return $ Just $ mtb {hoi4mtb_missions = ms ++ [mission] }
        handleBranch mtb stmt = throwError "Unhandled mission branch statement"

        handleMission :: (IsGameState (GameState g), MonadError Text m) =>
            HOI4Mission -> GenericStatement -> PPT g m HOI4Mission
        handleMission m [pdx| icon = $rhs |] = do -- <gfx> The icon to use for the mission
            return m { hoi4m_icon = rhs }
        handleMission m [pdx| generic = %rhs |] = return m -- <bool> Whether this mission is considered generic.
        handleMission m [pdx| position = !pos |] = -- <int> Which row the mission appears in. 1 is top.
            return m { hoi4m_position = pos }
        handleMission m [pdx| completed_by = %rhs |] = return m -- <date> Automatically completes mission in history.
        handleMission m [pdx| ai_weight = %rhs |] = return m -- ?
        handleMission m [pdx| ai_priority = %rhs |] = return m -- ?
        handleMission m [pdx| required_missions = @rhs |] = do -- <bare list> Which missions must be completed before this mission is active.
            prereq <- mapM requiredMission rhs
            return m { hoi4m_prerequisites = prereq }
        handleMission m [pdx| provinces_to_highlight = %rhs |] = return m -- <compund> Determines which provinces to highlight. Acts like all_province scope. Optional.
        handleMission m [pdx| trigger = @rhs |] = do -- <compound> Determines when the mission is completed. Country scope.
            return m { hoi4m_trigger = rhs }
        handleMission m [pdx| effect = @rhs |] = do -- <compound> The effect executed when the mission is claimed. Country scope.
            return m { hoi4m_effect = rhs }
        handleMission m stmt = throwError $ T.pack ("Unhandled mission statement: " ++ (show stmt))

        requiredMission :: (IsGameState (GameState g), MonadError Text m) => GenericStatement -> PPT g m Text
        requiredMission (StatementBare (GenericLhs rm [])) = return rm
        requiredMission stmt = throwError $ T.pack ("Unknown required_mission element: " ++ (show stmt))

parseHOI4MissionTree _ = return $ Left "Unsupported/invalid top-level LHS"


writeHOI4Missions :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Missions = do
    missions <- getMissions
    let features :: [Feature HOI4MissionTreeBranch]
        features = map (\mtb -> Feature { featurePath = Just (hoi4mtb_path mtb)
                                        , featureId = Just (hoi4mtb_id mtb)
                                        , theFeature = Right mtb })
                       (HM.elems missions)
    writeFeatures "missions" features pp_mtb
    where
        pp_mtb :: (HOI4Info g, Monad m) => HOI4MissionTreeBranch -> PPT g m Doc
        pp_mtb mtb = setCurrentFile (hoi4mtb_path mtb) $ do
            version <- gets (gameVersion . getSettings)
            potential <- mapM (scope HOI4Country . pp_script) (hoi4mtb_potential mtb)
            missionText <- mapM pp_m (hoi4mtb_missions mtb)
            return $ mconcat $ [
                        Doc.strictText $ "===" <> hoi4mtb_id mtb <> "===", PP.line, -- apparently there's no localization of the headline
                        "{{SVersion|", Doc.strictText version, "}}", PP.line
                    ] ++ (case potential of
                        Just pot -> ["<!-- Only available if:", PP.line, pot, PP.line, "-->", PP.line]
                        _ -> [])
                     ++ [
                     -- table header
                    "{| class=\"mildtable\"", PP.line] ++
                    (if useIconBox then
                        ["! width=\"325px\" | Mission", PP.line,
                         "! width=\"25%\" | Completion requirements", PP.line,
                         "! width=\"25%\" | Effects", PP.line,
                         "! Prerequisites", PP.line]
                    else
                        ["! width=\"2%\" | !! width=\"17%\" | Mission", PP.line,
                         "! width=\"20%\" | Completion requirements", PP.line,
                         "! width=\"30%\" | Effects", PP.line,
                         "! width=\"20%\" | Prerequisites", PP.line]
                    ) ++
                    missionText ++
                    -- table footer
                    ["|}", PP.line]

        -- TODO: Could create cache of icons instead of going through all missions every time
        findIcon :: [HOI4MissionTreeBranch] -> Text -> Text
        findIcon [] n = n
        findIcon (mtb:mtbs) n = case find (\m -> (hoi4m_id m) == n) (hoi4mtb_missions mtb) of
            Just m -> hoi4m_icon m
            Nothing -> findIcon mtbs n

        pp_prereq :: (HOI4Info g, Monad m) => Text -> PPT g m Doc
        pp_prereq req = do
            missions <- getMissions
            title <- getGameL10n $ req <> "_title"
            let icon = findIcon (HM.elems missions) req
            return $ mconcat [Doc.strictText $ "[[File:" <> icon <> ".png|24px]]" <> " [[#" <> (linkSyntax title) <> "|" <> title <> "]]", PP.line]

        pp_m :: (HOI4Info g, Monad m) => HOI4Mission -> PPT g m Doc
        pp_m m = do
            title <- getGameL10n $ (hoi4m_id m) <> "_title"
            desc <- getGameL10n $ (hoi4m_id m) <> "_desc"
            trigger <- scope HOI4Country (pp_script (hoi4m_trigger m))
            effect <- setIsInEffect True (scope HOI4Country (pp_script (hoi4m_effect m)))
            prereqs <- mapM pp_prereq (hoi4m_prerequisites m)
            return $ mconcat  $ [
                "|-",
                " <!-- {{mpos|x=", Doc.strictText $ T.pack (show $ hoi4m_slot m), "|y=", Doc.strictText $ T.pack (show $ hoi4m_position m), "|name=", Doc.strictText $ linkSyntax title ,"}} -->"] ++
                (if useIconBox then
                    [PP.line, "| {{iconbox|", Doc.strictText title, "|", Doc.strictText desc, "|image=", Doc.strictText (hoi4m_icon m), ".png}}", PP.line]
                else
                    [ "id=\"", Doc.strictText title, "\"", PP.line, "|[[File:", Doc.strictText (hoi4m_icon m), ".png]] || ", Doc.strictText title, PP.line]
                )
                ++ ["|", PP.line, trigger, PP.line,
                "|", PP.line, effect, PP.line,
                "| "] ++ prereqs ++ [PP.line]

        linkSyntax :: Text -> Text
        linkSyntax t = T.replace "]" ".5D" $ T.replace "[" ".5B" t
