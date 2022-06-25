{-|
Module      : HOI4.Settings
Description : Interface for Europa Universalis IV backend
-}
module HOI4.Settings (
        HOI4 (..)
    ,   module HOI4.Types
    ) where

import Debug.Trace (trace, traceM)

import Control.Monad (join, when, forM, filterM, void, unless)
import Control.Monad.Trans (MonadIO (..), liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT (..), modify, gets)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Maybe (listToMaybe, catMaybes)

import Data.Text (Text, toLower)
import Data.Monoid ((<>))

import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import QQ (pdx)
import FileIO (buildPath, readScript)
import SettingsTypes ( PPT, Settings (..), Game (..), L10nScheme (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10nIfPresent
                     , safeIndex, safeLast, CLArgs (..))
import HOI4.Types -- everything
--import Text.PrettyPrint.Leijen.Text (Doc)
--import qualified Text.PrettyPrint.Leijen.Text as PP
import Yaml (LocEntry (..))

-- Handlers
import HOI4.Decisions (parseHOI4Decisioncats
                      ,parseHOI4Decisions, writeHOI4Decisions)
import HOI4.IdeaGroups (parseHOI4IdeaGroups, writeHOI4IdeaGroups)
import HOI4.Modifiers (
--                    parseHOI4Modifiers, writeHOI4Modifiers,
                      parseHOI4OpinionModifiers, writeHOI4OpinionModifiers
--                    , parseHOI4ProvTrigModifiers, writeHOI4ProvTrigModifiers
                    )
--import HOI4.Missions (parseHOI4Missions , writeHOI4Missions)
import HOI4.Events (parseHOI4Events, writeHOI4Events
                   , findTriggeredEventsInEvents, findTriggeredEventsInDecisions
                   , findTriggeredEventsInOnActions
--                 , findTriggeredEventsInDisasters  , findTriggeredEventsInMissions
                   )
import HOI4.Extra (writeHOI4Extra, writeHOI4ExtraCountryScope, writeHOI4ExtraProvinceScope, writeHOI4ExtraModifier)

-- | Temporary (?) fix for HAW and UHW both localizing to "Hawai'i'"
-- Can be extended/removed as necessary
fixLocalization :: Settings -> Settings
fixLocalization s =
    let
        lan  = language s
        l10n = gameL10n s
        l10nForLan = HM.findWithDefault HM.empty lan l10n
        findKey key = content $ HM.findWithDefault (LocEntry 0 key) key l10nForLan
        hawLoc = findKey "HAW"
        newHavLoc = hawLoc <> " (HAW)"
        newL10n = HM.insert "HAW" (LocEntry 0 newHavLoc) l10nForLan
    in
        if hawLoc == findKey "UHW" then
            (trace $ "Note: Applying localization fix for HAW/UHW: " ++ (show hawLoc) ++ " -> " ++ (show newHavLoc)) $
                s { gameL10n = HM.insert lan newL10n l10n }
        else
            (trace "Warning: fixLocalization hack for HAW/UHW in HOI4/Settings.hs no longer needed!") $ s

-- | HOI4 game type. This is only interesting for its instances.
data HOI4 = HOI4
instance IsGame HOI4 where
    locScheme _  = L10nQYAML
    readScripts  = readHOI4Scripts
    parseScripts = parseHOI4Scripts
    writeScripts = writeHOI4Scripts
    data GameData HOI4 = HOI4D { hoi4d :: HOI4Data }
    data GameState HOI4 = HOI4S { hoi4s :: HOI4State }
    runWithInitState HOI4 settings st =
        void (runReaderT
                (runStateT st (HOI4D $ HOI4Data {
                    hoi4settings = fixLocalization settings
                ,   hoi4events = HM.empty
                ,   hoi4eventScripts = HM.empty
                ,   hoi4decisioncatScripts = HM.empty
                ,   hoi4decisioncats = HM.empty
                ,   hoi4decisions = HM.empty
                ,   hoi4decisionScripts = HM.empty
                ,   hoi4ideaGroups = HM.empty
                ,   hoi4ideaGroupScripts = HM.empty
--                ,   hoi4modifiers = HM.empty
--                ,   hoi4modifierScripts = HM.empty
                ,   hoi4opmods = HM.empty
                ,   hoi4opmodScripts = HM.empty
--                ,   hoi4missionScripts = HM.empty
--                ,   hoi4missions = HM.empty
                ,   hoi4eventTriggers = HM.empty
                ,   hoi4onactionsScripts = HM.empty
--                ,   hoi4disasterScripts = HM.empty
                ,   hoi4geoData = HM.empty
--                ,   hoi4provtrigmodifiers = HM.empty
--                ,   hoi4provtrigmodifierScripts = HM.empty
                ,   hoi4tradeNodes = HM.empty
                ,   hoi4extraScripts = HM.empty
                ,   hoi4extraScriptsCountryScope = HM.empty
                ,   hoi4extraScriptsProvinceScope = HM.empty
                ,   hoi4extraScriptsModifier = HM.empty
                }))
                (HOI4S $ HOI4State {
                    hoi4currentFile = Nothing
                ,   hoi4currentIndent = Nothing
                ,   hoi4scopeStack = []
                ,   hoi4IsInEffect = False
                }))
    type Scope HOI4 = HOI4Scope
    scope s = local $ \(HOI4S st) -> HOI4S $
        st { hoi4scopeStack = s : hoi4scopeStack st }
    getCurrentScope = asks $ listToMaybe . hoi4scopeStack . hoi4s
    getPrevScope = asks $ safeIndex 1 . hoi4scopeStack . hoi4s
    getRootScope = asks $ safeLast . hoi4scopeStack . hoi4s
    getScopeStack = asks $ hoi4scopeStack . hoi4s
    getIsInEffect = asks $ hoi4IsInEffect . hoi4s
    setIsInEffect b = local $ \(HOI4S st) -> HOI4S $ st { hoi4IsInEffect = b }

instance HOI4Info HOI4 where
    getEventTitle eid = do
        HOI4D ed <- get
        let evts = hoi4events ed
            mevt = HM.lookup eid evts
        case mevt of
            Nothing -> return Nothing
            Just evt -> case hoi4evt_title evt of
                [] -> return Nothing
                [HOI4EvtTitleSimple key] -> getGameL10nIfPresent key
                titles -> return Nothing
    getEventScripts = do
        HOI4D ed <- get
        return (hoi4eventScripts ed)
    setEventScripts scr = modify $ \(HOI4D ed) -> HOI4D $ ed {
            hoi4eventScripts = scr
        }
    getEvents = do
        HOI4D ed <- get
        return (hoi4events ed)
    getIdeaGroupScripts = do
        HOI4D ed <- get
        return (hoi4ideaGroupScripts ed)
    getIdeaGroups = do
        HOI4D ed <- get
        return (hoi4ideaGroups ed)
    getDecisioncatScripts = do
        HOI4D ed <- get
        return (hoi4decisioncatScripts ed)
    getDecisionScripts = do
        HOI4D ed <- get
        return (hoi4decisionScripts ed)
    getDecisioncats = do
        HOI4D ed <- get
        return (hoi4decisioncats ed)
    getDecisions = do
        HOI4D ed <- get
        return (hoi4decisions ed)
--    getModifierScripts = do
--        HOI4D ed <- get
--        return (hoi4modifierScripts ed)
--    getModifiers = do
--        HOI4D ed <- get
--        return (hoi4modifiers ed)
    getOpinionModifierScripts = do
        HOI4D ed <- get
        return (hoi4opmodScripts ed)
    getOpinionModifiers = do
        HOI4D ed <- get
        return (hoi4opmods ed)
--    getMissionScripts = do
--        HOI4D ed <- get
--        return (hoi4missionScripts ed)
--    getMissions = do
--        HOI4D ed <- get
--        return (hoi4missions ed)
    getEventTriggers = do
        HOI4D ed <- get
        return (hoi4eventTriggers ed)
    getOnActionsScripts = do
        HOI4D ed <- get
        return (hoi4onactionsScripts ed)
--    getDisasterScripts = do
--        HOI4D ed <- get
--        return (hoi4disasterScripts ed)
    getGeoData = do
        HOI4D ed <- get
        return (hoi4geoData ed)
--    getProvinceTriggeredModifierScripts = do
--        HOI4D ed <- get
--        return (hoi4provtrigmodifierScripts ed)
--    getProvinceTriggeredModifiers = do
--        HOI4D ed <- get
--        return (hoi4provtrigmodifiers ed)
    getTradeNodes = do
        HOI4D ed <- get
        return (hoi4tradeNodes ed)
    getExtraScripts = do
        HOI4D ed <- get
        return (hoi4extraScripts ed)
    getExtraScriptsCountryScope = do
        HOI4D ed <- get
        return (hoi4extraScriptsCountryScope ed)
    getExtraScriptsProvinceScope = do
        HOI4D ed <- get
        return (hoi4extraScriptsProvinceScope ed)
    getExtraScriptsModifier = do
        HOI4D ed <- get
        return (hoi4extraScriptsModifier ed)

instance IsGameData (GameData HOI4) where
    getSettings (HOI4D ed) = hoi4settings ed

instance IsGameState (GameState HOI4) where
    currentFile (HOI4S es) = hoi4currentFile es
    modifyCurrentFile cf (HOI4S es) = HOI4S $ es {
            hoi4currentFile = cf
        }
    currentIndent (HOI4S es) = hoi4currentIndent es
    modifyCurrentIndent ci (HOI4S es) = HOI4S $ es {
            hoi4currentIndent = ci
        }

-- | Read all scripts in a directory.
--
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readHOI4Scripts :: forall m. MonadIO m => PPT HOI4 m ()
readHOI4Scripts = do
    settings <- gets getSettings
    let readOneScript :: String -> String -> PPT HOI4 m (String, GenericScript)
        readOneScript category target = do
            content <- liftIO $ readScript settings target
            traceM (show target)
            when (null content) $
                liftIO $ hPutStrLn stderr $
                    "Warning: " ++ target
                        ++ " contains no scripts - failed parse? Expected feature type "
                        ++ category
            return (target, content)

        readHOI4Script :: String -> PPT HOI4 m (HashMap String GenericScript)
        readHOI4Script category = do
            let sourceSubdir = case category of
                    "policies" -> "common" </> "policies"
                    "ideagroups" -> "common" </> "ideas"
--                    "modifiers" -> "common" </> "event_modifiers"
                    "opinion_modifiers" -> "common" </> "opinion_modifiers"
                    "on_actions" -> "common" </> "on_actions"
--                    "disasters" -> "common" </> "disasters"
--                    "tradenodes" -> "common" </> "tradenodes"
--                    "trade_companies" -> "common" </> "trade_companies"
                    "colonial_regions" -> "common" </> "colonial_regions"
--                    "province_triggered_modifiers" -> "common" </> "province_triggered_modifiers"
                    "decisions" -> "common" </> "decisions"
                    "decisioncats" -> "common" </> "decisions" </> "categories"
                    "national_focus" -> "common" </> "national_focus"

                    _          -> category
                sourceDir = buildPath settings sourceSubdir
            files <- liftIO (filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                                     =<< getDirectoryContents sourceDir)
            results <- forM files $ \filename -> readOneScript category (sourceSubdir </> filename)
            return $ foldl (flip (uncurry HM.insert)) HM.empty results

        getOnlyLhs :: GenericStatement -> Maybe Text
        getOnlyLhs (Statement (GenericLhs lhs _) _ _) = Just (toLower lhs)
        getOnlyLhs stmt = (trace $ "Unsupported statement: " ++ (show stmt)) $ Nothing
{-
        toHashMap :: HOI4GeoType -> [Text] -> HashMap Text HOI4GeoType
        toHashMap gt l = foldr (\t -> HM.insert t gt) HM.empty l

        geoDirs = [ (HOI4GeoTradeCompany, "trade_companies")
                  , (HOI4GeoColonialRegion, "colonial_regions")
                  -- Tradenodes handled below
                  ]

        mapGeoFiles = [ (HOI4GeoArea, "area.txt")
                      , (HOI4GeoContinent, "continent.txt")
                      , (HOI4GeoRegion, "region.txt")
                      , (HOI4GeoSuperRegion, "superregion.txt")]

        readGeoData :: (HOI4GeoType, String) -> PPT HOI4 m (HashMap Text HOI4GeoType)
        readGeoData (gt, dir) = do
            hm <- readHOI4Script dir
            return $ toHashMap gt (catMaybes $ map getOnlyLhs (concat (HM.elems hm)))


        processTradeNode [pdx| $name = @scr |] = case findPrimary scr of
            Just id -> Just (id, name)
            _ -> (trace $ "Warning: Could not determine main province id for " ++ show name) $ Nothing
            where
                findPrimary :: GenericScript -> Maybe Int
                findPrimary ([pdx| location = !id |]:_) = Just id
                findPrimary (s:ss) = findPrimary ss
                findPrimary _ = Nothing
        processTradeNode stmt = (trace $ "Not handled in processTradeNode: " ++ show stmt) $ Nothing

        getFileFromOpts (ProcessFile f) = [f]
        getFileFromOpts _ = []

        getCountryScopeFileFromOpts (ProcessCountryScopeFile c) = [c]
        getCountryScopeFileFromOpts _ = []
        getProvinceScopeFileFromOpts (ProcessProvinceScopeFile s) = [s]
        getProvinceScopeFileFromOpts _ = []
        getModifierFileFromOpts (ProcessModifierFile m) = [m]
        getModifierFileFromOpts _ = []
-}
    ideaGroups <- readHOI4Script "ideagroups"
    decisioncats <- readHOI4Script "decisioncats"
    decisions <- readHOI4Script "decisions"
    events <- readHOI4Script "events"
--    modifiers <- readHOI4Script "modifiers"
    opinion_modifiers <- readHOI4Script "opinion_modifiers"
--    missions <- readHOI4Script "missions"
    on_actions <- readHOI4Script "on_actions"
--    disasters <- readHOI4Script "disasters"
--    provTrigModifiers <- readHOI4Script "province_triggered_modifiers"

--    extra <- mapM (readOneScript "extra") (concatMap getFileFromOpts (clargs settings))
--    extraCountryScope <- mapM (readOneScript "extraCountryScope") (concatMap getCountryScopeFileFromOpts (clargs settings))
--    extraProvinceScope <- mapM (readOneScript "extraProvinceScope") (concatMap getProvinceScopeFileFromOpts (clargs settings))
--    extraModifier <- mapM (readOneScript "extraModifier") (concatMap getModifierFileFromOpts (clargs settings))
    ---------------------
    -- Geographic data --
    ---------------------
    --
    -- Arguably this shouldn't be parsed here, but we don't care
    -- about the actual script data.
    --
    {-}
    geoData <- forM geoDirs readGeoData
    geoMapData <- forM mapGeoFiles  $ \(geoType, filename) -> do
        (_, d) <- readOneScript "map" (buildPath settings "map" </> filename)
        return $ toHashMap geoType (catMaybes $ map getOnlyLhs d)

    tradeNodeScripts <- readHOI4Script "tradenodes"
-}
    modify $ \(HOI4D s) -> HOI4D $ s {
            hoi4ideaGroupScripts = ideaGroups
        ,   hoi4decisioncatScripts = decisioncats
        ,   hoi4decisionScripts = decisions
        ,   hoi4eventScripts = events
--        ,   hoi4modifierScripts = modifiers
        ,   hoi4opmodScripts = opinion_modifiers
--        ,   hoi4missionScripts = missions
        ,   hoi4onactionsScripts = on_actions
--        ,   hoi4disasterScripts = disasters
--        ,   hoi4geoData = HM.union (foldl HM.union HM.empty geoData) (foldl HM.union HM.empty geoMapData)
--        ,   hoi4provtrigmodifierScripts = provTrigModifiers
--        ,   hoi4tradeNodes = HM.fromList (catMaybes (map processTradeNode (concatMap snd (HM.toList tradeNodeScripts))))
--        ,   hoi4extraScripts = foldl (flip (uncurry HM.insert)) HM.empty extra
--        ,   hoi4extraScriptsCountryScope = foldl (flip (uncurry HM.insert)) HM.empty extraCountryScope
--        ,   hoi4extraScriptsProvinceScope = foldl (flip (uncurry HM.insert)) HM.empty extraProvinceScope
--        ,   hoi4extraScriptsModifier = foldl (flip (uncurry HM.insert)) HM.empty extraModifier
        }


-- | Interpret the script ASTs as usable data.
parseHOI4Scripts :: Monad m => PPT HOI4 m ()
parseHOI4Scripts = do
    -- Need idea groups and modifiers before everything else
    ideaGroups <- parseHOI4IdeaGroups =<< getIdeaGroupScripts
--    modifiers <- parseHOI4Modifiers =<< getModifierScripts

    opinionModifiers <- parseHOI4OpinionModifiers =<< getOpinionModifierScripts
--    provTrigModifiers <- parseHOI4ProvTrigModifiers =<< getProvinceTriggeredModifierScripts
    decisioncats <- parseHOI4Decisioncats =<< getDecisioncatScripts
    decisions <- parseHOI4Decisions =<< getDecisionScripts
    events <- parseHOI4Events =<< getEventScripts
--    missions <- parseHOI4Missions =<< getMissionScripts
    on_actions <- getOnActionsScripts
--    disasters <- getDisasterScripts
    let te1 = findTriggeredEventsInEvents HM.empty (HM.elems events)
        te2 = findTriggeredEventsInDecisions te1 (HM.elems decisions)
        te3 = findTriggeredEventsInOnActions te2 (concat (HM.elems on_actions))
--        te4 = findTriggeredEventsInNationalFocuss te3 (concat (HM.elems national_focus)) -- not yet implemented
--        te4 = findTriggeredEventsInDisasters te3 (concat (HM.elems disasters))
--        te5 = findTriggeredEventsInMissions te4 (HM.elems missions)
    --traceM $ concat (map (\(k,v) -> (show k) ++ " -> " ++ show v ++ "\n") (HM.toList $ te5))
    modify $ \(HOI4D s) -> HOI4D $
            s { hoi4events = events
            ,   hoi4decisioncats = decisioncats
            ,   hoi4decisions = decisions
            ,   hoi4ideaGroups = ideaGroups
--            ,   hoi4modifiers = modifiers
            ,   hoi4opmods = opinionModifiers
--            ,   hoi4missions = missions
            ,   hoi4eventTriggers = te3
--            ,   hoi4provtrigmodifiers = provTrigModifiers
            }

-- | Output the game data as wiki text.
writeHOI4Scripts :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Scripts = do
    settings <- gets getSettings
    unless (Onlyextra `elem` (clargs settings)) $ do
        writeHOI4IdeaGroups
        writeHOI4Events
        writeHOI4Decisions
--        writeHOI4Missions
        writeHOI4OpinionModifiers
--        writeHOI4ProvTrigModifiers
    writeHOI4Extra
    writeHOI4ExtraCountryScope
    writeHOI4ExtraProvinceScope
    writeHOI4ExtraModifier
