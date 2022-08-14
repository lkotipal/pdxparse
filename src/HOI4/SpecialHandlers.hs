
module HOI4.SpecialHandlers (
        handleIdeas
    ,   handleTimedIdeas
    ,   handleSwapIdeas
    ,   handleModifier
    ,   modifierMSG
    ,   handleResearchBonus
    ,   handleTargetedModifier
    ,   handleEquipmentBonus
    ,   addDynamicModifier
    ) where

import Data.Char (toUpper, toLower, isUpper)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
--import Data.Set (Set)
import qualified Data.Set as S
import Data.Trie (Trie)
import qualified Data.Trie as Tr

import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.List (foldl', intersperse, intercalate)
import Data.Maybe

import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Monad (foldM, mplus, forM, join, when)
import Data.Foldable (fold)
import Data.Monoid ((<>))

import Abstract -- everything
import Doc (Doc)
import qualified Doc -- everything
import HOI4.Messages -- everything
import MessageTools (plural, iquotes, italicText, boldText
                    , colourNumSign, plainNumSign, plainPc, colourPc, reducedNum
                    , formatDays, formatHours)
import QQ -- everything
import SettingsTypes ( PPT, IsGameData (..), GameData (..), IsGameState (..), GameState (..)
                     , indentUp, indentDown, getCurrentIndent, withCurrentIndent, withCurrentIndentZero, withCurrentIndentCustom, alsoIndent, alsoIndent'
                     , getGameL10n, getGameL10nIfPresent, getGameL10nDefault, withCurrentFile
                     , unfoldM, unsnoc )
import HOI4.Templates
import {-# SOURCE #-} HOI4.Common (ppScript, ppMany, ppOne, extractStmt, matchLhsText)
import HOI4.Types -- everything
import Debug.Trace
import HOI4.Handlers -- everything
-----------------
-- handle idea --
-----------------

handleSwapIdeas :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleSwapIdeas stmt@[pdx| %_ = @scr |]
    = pp_si (parseTA "add_idea" "remove_idea" scr)
    where
        pp_si :: TextAtom -> PPT g m IndentedMessages
        pp_si ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                add_loc <- handleIdea True what
                remove_loc <- handleIdea False atom
                case (add_loc, remove_loc) of
                    (Just (addcategory, addideaIcon, addideaKey, addidea_loc, Just addeffectbox),
                     Just (category, ideaIcon, ideaKey, idea_loc, _)) ->
                        if addidea_loc == idea_loc then do
                                idmsg <- msgToPP $ MsgModifyIdea category ideaIcon ideaKey idea_loc
                                                    addcategory addideaIcon addideaKey addidea_loc
                                return $ idmsg ++ addeffectbox
                        else do
                            idmsg <- msgToPP $ MsgReplaceIdea category ideaIcon ideaKey idea_loc
                                                addcategory addideaIcon addideaKey addidea_loc
                            return $ idmsg ++ addeffectbox
                    (Just (addcategory, addideaIcon, addideaKey, addidea_loc, Nothing),
                     Just (category, ideaIcon, ideaKey, idea_loc, _)) ->
                        if addidea_loc == idea_loc then
                            msgToPP $ MsgModifyIdea category ideaIcon ideaKey idea_loc
                                addcategory addideaIcon addideaKey addidea_loc
                        else
                            msgToPP $ MsgReplaceIdea category ideaIcon ideaKey idea_loc
                                addcategory addideaIcon addideaKey addidea_loc
                    _ -> preStatement stmt
            _ -> preStatement stmt
handleSwapIdeas stmt = preStatement stmt

handleTimedIdeas :: forall g m. (HOI4Info g, Monad m) =>
        (Text -> Text -> Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> StatementHandler g m
handleTimedIdeas msg stmt@[pdx| %_ = @scr |]
    = pp_idda (parseTV "idea" "days" scr)
    where
        pp_idda :: TextValue -> PPT g m IndentedMessages
        pp_idda tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) -> do
                ideashandled <- handleIdea True what
                case ideashandled of
                    Just (category, ideaIcon, ideaKey, idea_loc, Just effectbox) -> do
                        idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc value
                        return $ idmsg ++ effectbox
                    Just (category, ideaIcon, ideaKey, idea_loc, Nothing) -> msgToPP $ msg category ideaIcon ideaKey idea_loc value
                    Nothing -> preStatement stmt
            _ -> preStatement stmt
handleTimedIdeas _ stmt = preStatement stmt

handleIdeas :: forall g m. (HOI4Info g, Monad m) =>
    Bool ->
    (Text -> Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
handleIdeas addIdea msg stmt@[pdx| $lhs = %idea |] = case idea of
    CompoundRhs ideas -> if length ideas == 1 then do
                ideashandled <- handleIdea addIdea (mconcat $ map getbareidea ideas)
                case ideashandled of
                    Just (category, ideaIcon, ideaKey, idea_loc, Just effectbox) -> do
                        idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc
                        return $ idmsg ++ effectbox
                    Just (category, ideaIcon, ideaKey, idea_loc, Nothing) -> msgToPP $ msg category ideaIcon ideaKey idea_loc
                    Nothing -> preStatement stmt
            else do
                ideashandle <- mapM (handleIdea addIdea . getbareidea) ideas
                let ideashandled = catMaybes ideashandle
                    ideasmsgd :: [(Text, Text, Text, Text, Maybe IndentedMessages)] -> PPT g m [IndentedMessages]
                    ideasmsgd ihs = mapM (\ih ->
                            let (category, ideaIcon, ideaKey, idea_loc, effectbox) = ih in
                            withCurrentIndent $ \i -> case effectbox of
                                    Just boxNS -> return ((i, msg category ideaIcon ideaKey idea_loc):boxNS)
                                    _-> return [(i, msg category ideaIcon ideaKey idea_loc)]
                                ) ihs
                ideasmsgdd <- ideasmsgd ideashandled
                return $ mconcat ideasmsgdd
    GenericRhs txt [] -> do
        ideashandled <- handleIdea addIdea txt
        case ideashandled of
            Just (category, ideaIcon, ideaKey, idea_loc, Just effectbox) -> do
                idmsg <- msgToPP $ msg category ideaIcon ideaKey idea_loc
                return $ idmsg ++ effectbox
            Just (category, ideaIcon, ideaKey, idea_loc, Nothing) -> msgToPP $ msg category ideaIcon ideaKey idea_loc
            Nothing -> preStatement stmt
    _ -> preStatement stmt
handleIdeas _ _ stmt = preStatement stmt

getbareidea :: GenericStatement -> Text
getbareidea (StatementBare (GenericLhs e [])) = e
getbareidea _ = "<!-- Check Script -->"

handleIdea :: (HOI4Info g, Monad m) =>
        Bool -> Text ->
           PPT g m (Maybe (Text, Text, Text, Text, Maybe IndentedMessages))
handleIdea addIdea ide = do
    ides <- getIdeas
    gfx <- getInterfaceGFX
    let midea = HM.lookup ide ides
    case midea of
        Nothing -> return Nothing -- unknown idea
        Just iidea -> do
            let ideaKey = id_name iidea
                ideaIcon = HM.findWithDefault "GFX_idea_unknown" (id_picture iidea) gfx
            idea_loc <- getGameL10n ideaKey
            category <- if id_category iidea == "country" then getGameL10n "FE_COUNTRY_SPIRIT" else getGameL10n $ id_category iidea
            effectbox <- modmessage iidea idea_loc ideaKey ideaIcon
            effectboxNS <- if id_category iidea == "country" && addIdea then return $ Just effectbox else return  Nothing
            return $ Just (category, ideaIcon, ideaKey, idea_loc, effectboxNS)

modmessage :: forall g m. (HOI4Info g, Monad m) => HOI4Idea -> Text -> Text -> Text -> PPT g m IndentedMessages
modmessage iidea idea_loc ideaKey ideaIcon = do
        curind <- getCurrentIndent
        curindent <- case curind of
            Just curindt -> return curindt
            _ -> return 1
        withCurrentIndentCustom 1 $ \_ -> do
            ideaDesc <- case id_desc_loc iidea of
                Just desc -> return $ Doc.nl2br desc
                _ -> return ""
            modifier <- maybe (return []) ppOne (id_modifier iidea)
            targeted_modifier <-
                maybe (return []) handleTargetedModifier (id_targeted_modifier iidea)
            research_bonus <- maybe (return []) ppOne (id_research_bonus iidea)
            equipment_bonus <- maybe (return []) ppOne (id_equipment_bonus iidea)
            let boxend = [(0, MsgEffectBoxEnd)]
            withCurrentIndentCustom curindent $ \_ -> do
                let ideamods = modifier ++ targeted_modifier ++ research_bonus ++ equipment_bonus ++ boxend
                return $ (0, MsgEffectBox idea_loc ideaKey ideaIcon ideaDesc) : ideamods


-----------------------
-- modifier handlers --
-----------------------
handleModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleModifier [pdx| %_ = @scr |] = fold <$> traverse modifierMSG scr
handleModifier stmt = preStatement stmt

modifierMSG :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
modifierMSG stmt@[pdx| $mod = !num|] = case HM.lookup (T.toLower mod) modifiersTable of
    Just (loc, msg) ->
        let bonus = num :: Double in
        numericLoc loc msg stmt
    Nothing -> preStatement stmt
modifierMSG stmt@[pdx| $mod = $var|] = case HM.lookup (T.toLower mod) modifiersTable of
    Just (loc, msg) -> do
        locced <- getGameL10n loc
        msgToPP $ MsgModifierVar locced var
    Nothing -> preStatement stmt
modifierMSG stmt = preStatement stmt

handleResearchBonus :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleResearchBonus [pdx| %_ = @scr |] = fold <$> traverse handleResearchBonus' scr
    where
        handleResearchBonus' stmt@[pdx| $tech = !num |] = let bonus = num :: Double in numericLoc (T.toLower tech <> "_research") MsgModifierPcPosReduced stmt
        handleResearchBonus' scr = preStatement scr
handleResearchBonus stmt = preStatement stmt

handleTargetedModifier :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleTargetedModifier stmt@[pdx| %_ = @scr |] = do
    let (tag, rest) = extractStmt (matchLhsText "tag") scr
    tagmsg <- case tag of
        Just [pdx| tag = $country |] -> plainMsg' . (<> ":") =<< flagText (Just HOI4Country) country
        _ -> plainMsg' "CHECK SCRIPT"
    modmsg <- fold <$> traverse modifierMSG rest
    return $ tagmsg : modmsg
        where
            modifierMSG stmt@[pdx| $mod = !num |] = case HM.lookup (T.toLower mod) targetedModifiersTable of
                Just (loc, msg) -> let bonus = num :: Double in numericLoc loc msg stmt
                Nothing -> preStatement stmt
            modifierMSG stmt = preStatement stmt
handleTargetedModifier stmt = preStatement stmt


handleEquipmentBonus :: forall g m. (HOI4Info g, Monad m) =>
        StatementHandler g m
handleEquipmentBonus stmt@[pdx| %_ = @scr |] = fold <$> traverse modifierEquipMSG scr
        where
            modifierEquipMSG [pdx| $tech = @scr |] = do
                let (_, rest) = extractStmt (matchLhsText "instant") scr
                techmsg <- plainMsg' . (<> ":") =<< getGameL10n tech
                modmsg <- fold <$> traverse modifierEquipMSG rest
                return $ techmsg : modmsg
            modifierEquipMSG stmt = preStatement stmt

            modifierMSG stmt@[pdx| $mod = !num |] = case HM.lookup (T.toLower mod) equipmentBonusTable of
                Just (loc, msg) -> let bonus = num :: Double in numericLoc loc msg stmt
                Nothing -> preStatement stmt
            modifierMSG stmt = preStatement stmt
handleEquipmentBonus stmt = preStatement stmt

-- | Handlers for numeric statements with icons
modifiersTable :: HashMap Text (Text, Text -> Double -> ScriptMessage)
modifiersTable = HM.fromList
        [
        --general modifiers
         ("monthly_population"              , ("MODIFIER_GLOBAL_MONTHLY_POPULATION", MsgModifierPcPosReduced))
        ,("nuclear_production_factor"       , ("MODIFIER_NUCLEAR_PRODUCTION_FACTOR", MsgModifierPcPosReduced))
        ,("research_sharing_per_country_bonus" , ("MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS", MsgModifierPcPosReduced))
        --,("research_sharing_per_country_bonus_factor" , ("MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS_FACTOR", MsgModifier)) -- % pos
        ,("research_speed_factor"           , ("MODIFIER_RESEARCH_SPEED_FACTOR", MsgModifierPcPosReduced))
        ,("local_resources_factor"          , ("MODIFIER_LOCAL_RESOURCES_FACTOR", MsgModifierPcPosReduced))
            -- Politics modifiers
        ,("min_export"                      , ("MODIFIER_MIN_EXPORT_FACTOR", MsgModifierPcReducedSign)) -- yellow
        ,("trade_opinion_factor"            , ("MODIFIER_TRADE_OPINION_FACTOR", MsgModifierPcReducedSign))
        ,("economy_cost_factor"             , ("economy_cost_factor", MsgModifierPcNegReduced))
        ,("mobilization_laws_cost_factor"   , ("mobilization_laws_cost_factor", MsgModifierPcNegReduced))
        ,("political_advisor_cost_factor"   , ("political_advisor_cost_factor", MsgModifierPcNegReduced))
        ,("trade_laws_cost_factor"          , ("trade_laws_cost_factor", MsgModifierPcNegReduced))
        ,("improve_relations_maintain_cost_factor" , ("MODIFIER_IMPROVE_RELATIONS_MAINTAIN_COST_FACTOR", MsgModifierPcNegReduced))
        ,("party_popularity_stability_factor" , ("MODIFIER_STABILITY_POPULARITY_FACTOR", MsgModifierPcPosReduced))
        ,("political_power_cost"            , ("MODIFIER_POLITICAL_POWER_COST", MsgModifierColourNeg))
        ,("political_power_gain"            , ("MODIFIER_POLITICAL_POWER_GAIN", MsgModifierColourPos))
        ,("political_power_factor"          , ("MODIFIER_POLITICAL_POWER_FACTOR", MsgModifierPcPosReduced))
        ,("stability_factor"                , ("MODIFIER_STABILITY_FACTOR", MsgModifierPcPosReduced))
        ,("stability_weekly"                , ("MODIFIER_STABILITY_WEEKLY", MsgModifierPcPosReduced))
        ,("stability_weekly_factor"         , ("MODIFIER_STABILITY_WEEKLY_FACTOR", MsgModifierPcPosReduced))
        ,("war_support_factor"              , ("MODIFIER_WAR_SUPPORT_FACTOR", MsgModifierPcPosReduced))
        ,("war_support_weekly"              , ("MODIFIER_WAR_SUPPORT_WEEKLY", MsgModifierPcPosReduced))
        ,("war_support_weekly_factor"       , ("MODIFIER_WAR_SUPPORT_WEEKLY_FACTOR", MsgModifierPcPosReduced))
        ,("drift_defence_factor"            , ("MODIFIER_DRIFT_DEFENCE_FACTOR", MsgModifierPcPosReduced))
        ,("communism_drift"                 , ("communism_drift", MsgModifierColourPos))
        ,("democratic_drift"                , ("democratic_drift", MsgModifierColourPos))
        ,("fascism_drift"                   , ("fascism_drift", MsgModifierColourPos))
        ,("neutrality_drift"                , ("neutrality_drift", MsgModifierColourPos))
        ,("communism_acceptance"            , ("communism_acceptance", MsgModifierColourPos))
        ,("democratic_acceptance"           , ("democratic_acceptance", MsgModifierColourPos))
        ,("fascism_acceptance"              , ("fascism_acceptance", MsgModifierColourPos))
        ,("neutrality_acceptance"           , ("neutrality_acceptance", MsgModifierColourPos))
            -- Diplomacy
        ,("generate_wargoal_tension"        , ("MODIFIER_GENERATE_WARGOAL_TENSION_LIMIT", MsgModifierPcReduced)) -- yellow
        ,("guarantee_cost"                  , ("MODIFIER_GUARANTEE_COST", MsgModifierPcNegReduced))
        ,("guarantee_tension"               , ("MODIFIER_GUARANTEE_TENSION_LIMIT", MsgModifierPcNegReduced))
        ,("join_faction_tension"            , ("MODIFIER_JOIN_FACTION_TENSION_LIMIT", MsgModifierPcNegReduced))
        ,("justify_war_goal_time"           , ("MODIFIER_JUSTIFY_WAR_GOAL_TIME", MsgModifierPcNegReduced))
        ,("lend_lease_tension"              , ("MODIFIER_LEND_LEASE_TENSION_LIMIT", MsgModifierPcNegReduced))
        --,("opinion_gain_monthly"            , ("MODIFIER_OPINION_GAIN_MONTHLY", MsgModifier)) -- flat pos
        ,("opinion_gain_monthly_factor"     , ("MODIFIER_OPINION_GAIN_MONTHLY_FACTOR", MsgModifierPcPosReduced))
        ,("opinion_gain_monthly_same_ideology_factor" , ("MODIFIER_OPINION_GAIN_MONTHLY_SAME_IDEOLOGY_FACTOR", MsgModifierPcPosReduced))
        ,("request_lease_tension"           , ("MODIFIER_REQUEST_LEASE_TENSION_LIMIT", MsgModifierPcNegReduced))
        ,("surrender_limit"                 , ("MODIFIER_SURRENDER_LIMIT", MsgModifierPcPosReduced))
        ,("send_volunteer_divisions_required" , ("MODIFIER_SEND_VOLUNTEER_DIVISIONS_REQUIRED", MsgModifierPcNegReduced))
        ,("send_volunteer_size"             , ("MODIFIER_SEND_VOLUNTEER_SIZE", MsgModifierColourPos))
        ,("send_volunteers_tension"         , ("MODIFIER_SEND_VOLUNTEERS_TENSION_LIMIT", MsgModifierPcNegReduced))
            -- autonomy
        ,("subjects_autonomy_gain"          , ("MODIFIER_AUTONOMY_SUBJECT_GAIN", MsgModifierColourPos))
            -- Governments in exile
            -- Equipment
        ,("equipment_conversion_speed"      , ("EQUIPMENT_CONVERSION_SPEED_MODIFIERS", MsgModifierPcPosReduced))
        ,("equipment_upgrade_xp_cost"       , ("MODIFIER_EQUIPMENT_UPGRADE_XP_COST", MsgModifierPcNegReduced))
        ,("license_armor_purchase_cost"     , ("MODIFIER_LICENSE_ARMOR_PURCHASE_COST", MsgModifierPcNegReduced))
        ,("griffon_air_licenses"            , ("MODIFIER_LICENSE_AIR_PURCHASE_COST", MsgModifierPcNegReduced))
        ,("production_factory_efficiency_gain_factor" , ("MODIFIER_PRODUCTION_FACTORY_EFFICIENCY_GAIN_FACTOR", MsgModifierPcPosReduced))
        ,("production_factory_max_efficiency_factor" , ("MODIFIER_PRODUCTION_FACTORY_MAX_EFFICIENCY_FACTOR", MsgModifierPcPosReduced))
        ,("production_factory_start_efficiency_factor" , ("MODIFIER_PRODUCTION_FACTORY_START_EFFICIENCY_FACTOR", MsgModifierPcPosReduced))
            -- Military outside of combat
        ,("command_power_gain"              , ("MODIFIER_COMMAND_POWER_GAIN", MsgModifierColourPos))
        ,("command_power_gain_mult"         , ("MODIFIER_COMMAND_POWER_GAIN_MULT", MsgModifierPcPosReduced))
        ,("conscription"                    , ("MODIFIER_CONSCRIPTION_FACTOR", MsgModifierPcReducedSign)) --yellow
        ,("conscription_factor"             , ("MODIFIER_CONSCRIPTION_TOTAL_FACTOR", MsgModifierPcPosReduced))
        ,("training_time_factor"            , ("MODIFIER_TRAINING_TIME_FACTOR", MsgModifierPcNegReduced))
        ,("max_command_power"               , ("MODIFIER_MAX_COMMAND_POWER", MsgModifierColourPos))
        ,("max_command_power_mult"          , ("MODIFIER_MAX_COMMAND_POWER_MULT", MsgModifier))  -- % pos
        ,("training_time_army_factor"       , ("MODIFIER_TRAINING_TIME_ARMY_FACTOR", MsgModifierPcReducedSign)) --yellow
        ,("cat_battlefield_support_cost_factor" , ("cat_battlefield_support_cost_factor", MsgModifierPcNegReduced))
        ,("unit_light_armor_design_cost_factor" , ("modifier_unit_light_armor_design_cost_factor", MsgModifierPcNegReduced))
        ,("weekly_manpower"                 , ("MODIFIER_WEEKLY_MANPOWER", MsgModifierColourPos))
            -- Fuel
        --,("base_fuel_gain"                  , ("MODIFIER_BASE_FUEL_GAIN_ADD", MsgModifier)) -- flat neutr?
        --,("base_fuel_gain_factor"           , ("MODIFIER_BASE_FUEL_GAIN_FACTOR", MsgModifier)) -- % pos
        --,("fuel_cost"                       , ("MODIFIER_FUEL_COST", MsgModifier)) -- flat neg
        --,("fuel_gain"                       , ("MODIFIER_FUEL_GAIN_ADD", MsgModifier)) -- flat pos
        ,("fuel_gain_factor"                , ("MODIFIER_MAX_FUEL_FACTOR", MsgModifierPcPosReduced))
        --,("max_fuel"                        , ("MODIFIER_MAX_FUEL_ADD", MsgModifier)) -- flat
        ,("max_fuel_factor"                 , ("MODIFIER_MAX_FUEL_FACTOR", MsgModifierPcPosReduced))
            -- buildings
        ,("civilian_factory_use"            , ("MODIFIER_CIVILIAN_FACTORY_USE", MsgModifierPc)) -- yellow
        ,("industry_free_repair_factor"     , ("MODIFIER_INDUSTRY_FREE_REPAIR_FACTOR", MsgModifierPcPosReduced))
        ,("production_speed_air_base_factor" , ("modifier_production_speed_air_base_factor", MsgModifierPcPosReduced))
        ,("production_speed_anti_air_building_factor" , ("modifier_production_speed_anti_air_building_factor", MsgModifierPcPosReduced))
        ,("production_speed_arms_factory_factor" , ("modifier_production_speed_arms_factory_factor", MsgModifierPcPosReduced))
        ,("production_speed_bunker_factor"  , ("modifier_production_speed_bunker_factor", MsgModifierPcPosReduced))
        ,("production_speed_coastal_bunker_factor" , ("modifier_production_speed_coastal_bunker_factor", MsgModifierPcPosReduced))
        ,("production_speed_dockyard_factor" , ("modifier_production_speed_dockyard_factor", MsgModifierPcPosReduced))
        ,("production_speed_infrastructure_factor" , ("modifier_production_speed_infrastructure_factor", MsgModifierPcPosReduced))
        ,("production_speed_industrial_complex_factor" , ("modifier_production_speed_industrial_complex_factor", MsgModifierPcPosReduced))
        ,("production_speed_nuclear_reactor_factor" , ("modifier_production_speed_nuclear_reactor_factor", MsgModifierPcPosReduced))
        ,("production_speed_radar_station_factor" , ("modifier_production_speed_radar_station_factor", MsgModifierPcPosReduced))
        ,("production_speed_rail_way_factor" , ("modifier_production_speed_rail_way_factor", MsgModifierPcPosReduced))
        ,("production_speed_rocket_site_factor" , ("modifier_speed_rocket_site_factor", MsgModifierPcPosReduced))
        ,("production_speed_synthetic_refinery_factor" , ("modifier_production_speed_synthetic_refinery_factor", MsgModifierPcPosReduced))
        ,("consumer_goods_factor"           , ("MODIFIER_CONSUMER_GOODS_FACTOR", MsgModifierPcReduced))
        ,("conversion_cost_civ_to_mil_factor" , ("MODIFIER_CONVERSION_COST_CIV_TO_MIL_FACTOR", MsgModifierPcNegReduced))
        ,("conversion_cost_mil_to_civ_factor" , ("MODIFIER_CONVERSION_COST_MIL_TO_CIV_FACTOR", MsgModifierPcNegReduced))
        --,("global_building_slots"           , ("MODIFIER_GLOBAL_BUILDING_SLOTS", MsgModifierPcPosReduced)) -- flat
        ,("global_building_slots_factor"    , ("MODIFIER_GLOBAL_BUILDING_SLOTS_FACTOR", MsgModifierPcPosReduced))
        ,("industrial_capacity_dockyard"    , ("MODIFIER_INDUSTRIAL_CAPACITY_DOCKYARD_FACTOR", MsgModifierPcPosReduced))
        ,("industrial_capacity_factory"     , ("MODIFIER_INDUSTRIAL_CAPACITY_FACTOR", MsgModifierPcPosReduced))
        ,("industry_repair_factor"          , ("MODIFIER_INDUSTRY_REPAIR_FACTOR", MsgModifierPcPosReduced))
        ,("line_change_production_efficiency_factor" , ("MODIFIER_LINE_CHANGE_PRODUCTION_EFFICIENCY_FACTOR", MsgModifierPcPosReduced))
        ,("production_oil_factor"           , ("MODIFIER_PRODUCTION_OIL_FACTOR", MsgModifierPcPosReduced))
        ,("production_speed_buildings_factor" , ("MODIFIER_PRODUCTION_SPEED_BUILDINGS_FACTOR", MsgModifierPcPosReduced))
            -- resistance and compliance
        ,("civilian_intel_to_others"        , ("MODIFIER_CIVILIAN_INTEL_TO_OTHERS", MsgModifierPcNeg))
            -- Intelligence
        ,("foreign_subversive_activites"    , ("MODIFIER_FOREIGN_SUBVERSIVE_ACTIVITIES", MsgModifierPcNegReduced))
            -- Operatives
        ,("enemy_operative_detection_chance_factor" , ("MODIFIER_ENEMY_OPERATIVE_DETECTION_CHANCE_FACTOR", MsgModifierPcPosReduced))
            -- AI
        ,("ai_focus_defense_factor"         , ("MODIFIER_AI_FOCUS_DEFENSE_FACTOR", MsgModifierPcReducedSign))
        ,("ai_focus_peaceful_factor"        , ("MODIFIER_AI_FOCUS_PEACEFUL_FACTOR", MsgModifierPcReducedSign))
        ,("ai_get_ally_desire_factor"       , ("MODIFIER_AI_GET_ALLY_DESIRE_FACTOR", MsgModifierSign))
        ,("ai_badass_factor"                , ("MODIFIER_AI_BADASS_FACTOR", MsgModifierPcReduced))
            -- Unit Leaders
        ,("military_leader_cost_factor"     , ("MODIFIER_MILITARY_LEADER_COST_FACTOR", MsgModifierPcNegReduced))
            -- General Combat
        ,("offence"                         , ("MODIFIER_OFFENCE", MsgModifierPcPosReduced))
        ,("defence"                         , ("MODIFIER_DEFENCE", MsgModifierPcPosReduced))
            -- Land Combat
        ,("army_attack_factor"              , ("MODIFIERS_ARMY_ATTACK_FACTOR", MsgModifierPcPosReduced))
        ,("army_core_attack_factor"         , ("MODIFIERS_ARMY_CORE_ATTACK_FACTOR", MsgModifierPcPosReduced))
        ,("army_defence_factor"             , ("MODIFIERS_ARMY_DEFENCE_FACTOR", MsgModifierPcPosReduced))
        ,("army_core_defence_factor"        , ("MODIFIERS_ARMY_CORE_DEFENCE_FACTOR", MsgModifierPcPosReduced))
        ,("army_morale_factor"              , ("MODIFIER_ARMY_MORALE_FACTOR", MsgModifierPcPosReduced))
        ,("experience_gain_motorized_combat_factor" , ("modifier_experience_gain_motorized_combat_factor", MsgModifierPcPosReduced))
        ,("max_dig_in_factor"               , ("MODIFIER_MAX_DIG_IN_FACTOR", MsgModifierPcPosReduced))
        ,("land_night_attack"               , ("MODIFIER_LAND_NIGHT_ATTACK", MsgModifierPcPosReduced))
            -- Naval combat
            -- Air combat
            -- targeted
        -- State Scope
        ,("compliance_gain"                 , ("MODIFIER_GLOBAL_NON_CORE_MANPOWER", MsgModifierPcReduced))
        ,("mobilization_speed"              , ("MODIFIER_MOBILIZATION_SPEED", MsgModifierPcPosReduced))
        ,("non_core_manpower"               , ("MODIFIER_GLOBAL_NON_CORE_MANPOWER", MsgModifierPcPosReduced))
        ,("resistance_damage_to_garrison"   , ("MODIFIER_RESISTANCE_DAMAGE_TO_GARRISONS", MsgModifierPcNegReduced))
        -- Unit Leader Scope
        ,("trait_panzer_leader_xp_gain_factor" , ("modifier_trait_panzer_leader_xp_gain_factor", MsgModifierPcPosReduced))
        ]

-- | Handlers for numeric statements with icons
targetedModifiersTable :: HashMap Text (Text, Text -> Double -> ScriptMessage)
targetedModifiersTable = HM.fromList
        []

-- | Handlers for numeric statements with icons
equipmentBonusTable :: HashMap Text (Text, Text -> Double -> ScriptMessage)
equipmentBonusTable = HM.fromList
        [("build_cost_ic"         ,("STAT_COMMON_BUILD_COST_IC", MsgModifierPcNegReduced))
        ]


-------------------------------------------------
-- Handler for add_dynamic_modifier --
-------------------------------------------------
data AddDynamicModifier = AddDynamicModifier
    { adm_modifier :: Text
    , adm_scope :: Either Text (Text, Text)
    , adm_days :: Maybe Double
    }
newADM :: AddDynamicModifier
newADM = AddDynamicModifier undefined (Left "ROOT") Nothing
addDynamicModifier :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
addDynamicModifier stmt@[pdx| %_ = @scr |] =
    pp_adm (foldl' addLine newADM scr)
    where
        addLine adm [pdx| modifier = $mod |] = adm { adm_modifier = mod }
        addLine adm [pdx| scope = $tag |] = adm { adm_scope = Left tag }
        addLine adm [pdx| scope = $vartag:$var |] = adm { adm_scope = Right (vartag, var) }
        addLine adm [pdx| days = !amt |] = adm { adm_days = Just amt }
        addLine adm stmt = trace ("Unknown in add_dynamic_modifier: " ++ show stmt) adm
        pp_adm adm = do
            let days = maybe "" formatDays (adm_days adm)
            mmod <- HM.lookup (adm_modifier adm) <$> getDynamicModifiers
            dynflag <- eflag (Just HOI4Country) $ adm_scope adm
            let dynflagd = fromMaybe "<!-- Check Script -->" dynflag
            case mmod of
                Just mod -> withCurrentIndent $ \i -> do
                    effect <- fold <$> traverse modifierMSG (dmodEffects mod)
                    trigger <- indentUp $ ppMany (dmodEnable mod)
                    let name = dmodLocName mod
                        locName = maybe ("<tt>" <> adm_modifier adm <> "</tt>") (Doc.doc2text . iquotes) name
                    return $ ((i, MsgAddDynamicModifier locName dynflagd days) : effect) ++ (if null trigger then [] else (i+1, MsgLimit) : trigger)
                Nothing -> trace ("add_dynamic_modifier: Modifier " ++ T.unpack (adm_modifier adm) ++ " not found") $ preStatement stmt
addDynamicModifier stmt = trace ("Not handled in addDynamicModifier: " ++ show stmt) $ preStatement stmt
