{-# LANGUAGE TupleSections #-}
{-|
Module      : HOI4.Common
Description : Message handler for Europa Hearts of Iron IV
-}
module HOI4.Common (
        ppScript
    ,   pp_mtth
    ,   ppOne
    ,   ppMany
    ,   iconKey, iconFile, iconFileB
    ,   AIWillDo (..), AIModifier (..)
    ,   ppAiWillDo, ppAiMod
    ,   extractStmt, matchLhsText, matchExactText
    ,   module HOI4.Types
    ) where

import Debug.Trace (trace, traceM)
import Yaml (LocEntry (..))

import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Monad (liftM, MonadPlus (..), forM, foldM, join {- temp -}, when)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..), gets)

import Data.Char (isUpper, toUpper, toLower)
import Data.List (foldl', intersperse)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Foldable (fold)
import Data.Void (Void)

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

-- TODO: get rid of these, do icon key lookups from another module
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Trie (Trie)
import qualified Data.Trie as Tr

import qualified Data.Set as S

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import HOI4.Messages -- everything
import MessageTools (plural)
import QQ (pdx)
import SettingsTypes -- everything
import HOI4.Handlers -- everything
import HOI4.Types -- everything

-- no particular order from here... TODO: organize this!

-- | Format a script as wiki text.
ppScript :: (HOI4Info g, Monad m) =>
    GenericScript -> PPT g m Doc
ppScript [] = return "(Nothing)"
ppScript script = imsg2doc =<< ppMany script

flagTextMaybe :: (HOI4Info g, Monad m) => Text -> PPT g m (Text,Text)
flagTextMaybe = fmap (mempty,) . flagText (Just HOI4Country)

-- | Extract the appropriate message(s) from a script.
ppMany :: (HOI4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppMany scr = indentUp (concat <$> mapM ppOne scr)

-- | Table of handlers for statements. Dispatch on strings is /much/ quicker
-- using a lookup table than a huge @case@ expression, which uses @('==')@ on
-- each one in turn.
--
-- When adding a new statement handler, add it to one of the sections in
-- alphabetical order if possible, and use one of the generic functions for it
-- if applicable.
ppHandlers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
ppHandlers = foldl' Tr.unionL Tr.empty
    [ handlersRhsIrrelevant
    , handlersNumeric
    , handlersNumericCompare
    , handlersNumericIcons
    , handlersModifiers
    , handlersCompound
    , handlersLocRhs
    , handlersState
    , handlersFlagOrstate
    , handlersNumericOrFlag
    , handlersAdvisorId
    , handlersTypewriter
    , handlersSimpleIcon
    , handlersSimpleFlag
    , handlersFlagOrYesNo
    , handlersIconFlagOrPronoun
    , handlersYesNo
    , handlersNumericOrTag
    , handlersNumStates
    , handlersTextValue
    , handlersTextAtom
    , handlersSpecialComplex
    , handlersIdeas
    , handlersMisc
    , handlersIgnored
    ]

-- | Handlers for statements where RHS is irrelevant (usually "yes")
handlersRhsIrrelevant :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersRhsIrrelevant = Tr.fromList
        [("dismantle_faction"       , rhsAlwaysYes MsgDismantleFaction)
        ,("drop_cosmetic_tag"       , rhsAlwaysYes MsgDropCosmeticTag)
        ,("kill_country_leader"    , rhsAlwaysYes MsgKillCountryLeader)
        ,("retire_country_leader"       , rhsAlwaysYes MsgRetireCountryLeader)
        ]

-- | Handlers for numeric statements
handlersNumeric :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumeric = Tr.fromList
        [("add_research_slot"                , numeric MsgAddResearchSlot)
        ,("add_threat"                       , numeric MsgAddThreat)
        ,("reset_province_name"              , numeric MsgResetProvinceName)
        ,("set_compliance"                   , numeric MsgSetCompliance)
        ]

-- | Handlers for numeric statements that compare
handlersNumericCompare :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericCompare = Tr.fromList
        [("air_base"                         , numericCompare "more than" "less than" MsgAirBase MsgAirBaseVar)
        ,("amount_research_slots"            , numericCompare "more than" "less than" MsgAmountResearchSlots MsgAmountResearchSlotsVar)
        ,("any_war_score"                    , numericCompare "over" "under" MsgAnyWarScore MsgAnyWarScoreVar)
        ,("arms_factory"                     , numericCompare "more than" "less than" MsgArmsFactory MsgArmsFactoryVar)
        ,("compare_autonomy_progress_ratio"  , numericCompare "over" "under" MsgCompareAutonomyProgressRatio MsgCompareAutonomyProgressRatioVar)
        ,("dockyard"                         , numericCompare "more than" "less than" MsgDockyard MsgDockyardVar)
        ,("enemies_strength_ratio"           , numericCompare "over" "under" MsgEnemiesStrengthRatio MsgEnemiesStrengthRatioVar)
        ,("has_added_tension_amount"         , numericCompare "more than" "less than" MsgHasAddedTensionAmount MsgHasAddedTensionAmountVar)
        ,("has_army_manpower"                , numericCompareCompound "at least" "at most" MsgHasArmyManpower MsgHasArmyManpowerVar)
        ,("has_manpower"                     , numericCompare "more than" "less than" MsgHasManpower MsgHasManpowerVar)
        ,("has_stability"                    , numericCompare "more than" "less than" MsgHasStability MsgHasStabilityVar)
        ,("has_war_support"                  , numericCompare "more than" "less than" MsgHasWarSupport MsgHasWarSupportVar)
        ,("num_of_factories"                 , numericCompare "more than" "less than" MsgNumOfFactories MsgNumOfFactoriesVar)
        ,("surrender_progress"               , numericCompare "more than" "less than" MsgSurrenderProgress MsgSurrenderProgressVar)
        ,("threat"                           , numericCompare "more than" "less than" MsgThreat MsgThreatVar)
        ,("fascism"                          , numericCompare "more than" "less than" MsgFascismCompare MsgFascismCompareVar)
        ,("democratic"                       , numericCompare "more than" "less than" MsgDemocraticCompare MsgDemocraticCompareVar)
        ,("communism"                        , numericCompare "more than" "less than" MsgCommunismCompare MsgCommunismCompareVar)
        ,("neutrality"                       , numericCompare "more than" "less than" MsgNeutralityCompare MsgNeutralityCompareVar)
        ]

-- | Handlers for numeric statements with icons
handlersNumericIcons :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericIcons = Tr.fromList
        [("add_manpower"             , numericIconLoc "manpower" "MANPOWER" MsgGainManpower)
        ,("add_political_power"      , numericIcon "political power" MsgGainPoliticalPower)
        ,("add_stability"            , numericIconLoc "stability" "STABILITY" MsgGainLocPC)
        ,("add_war_support"          , numericIconLoc "war support" "WAR_SUPPORT" MsgGainLocPC)
        ,("air_experience"           , numericIconLoc "air exp" "AIR_EXPERIENCE" MsgAirExperience)
        ,("army_experience"          , numericIconLoc "army exp" "ARMY_EXPERIENCE" MsgArmyExperience)
        ,("navy_experience"          , numericIconLoc "navy exp" "NAVY_EXPERIENCE" MsgNavyExperience)
        ]

-- | Handlers for statements pertaining to modifiers
handlersModifiers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersModifiers = Tr.fromList
        [("add_dynamic_modifier"           , addDynamicModifier)
        -- Used in ideas and other bonuses, omit "gain/lose" in l10n
        --Country Scope
            --general modifiers
        ,("monthly_population"          , numericLoc "MODIFIER_GLOBAL_MONTHLY_POPULATION" MsgModifierPcPosReduced)
        ,("nuclear_production_factor"   , numericLoc "MODIFIER_NUCLEAR_PRODUCTION_FACTOR" MsgModifierPcPosReduced)
        ,("research_sharing_per_country_bonus" , numericLoc "MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS" MsgModifierPcPosReduced)
        ,("research_sharing_per_country_bonus_factor" , numericLoc "MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS_FACTOR" MsgModifier) -- % pos
        ,("research_speed_factor"       , numericLoc "MODIFIER_RESEARCH_SPEED_FACTOR" MsgModifierPcPosReduced)
        ,("local_resources_factor"      , numericLoc "MODIFIER_LOCAL_RESOURCES_FACTOR" MsgModifierPcPosReduced)
            -- Politics modifiers
        ,("min_export"                  , numericLoc "MODIFIER_MIN_EXPORT_FACTOR" MsgModifierPcReducedSign) -- yellow
        ,("trade_opinion_factor"        , numericLoc "MODIFIER_TRADE_OPINION_FACTOR" MsgModifierPcReducedSign)
        ,("economy_cost_factor"         , numericLoc "economy_cost_factor" MsgModifierPcNegReduced)
        ,("mobilization_laws_cost_factor" , numericLoc "mobilization_laws_cost_factor" MsgModifierPcNegReduced)
        ,("political_advisor_cost_factor" , numericLoc "political_advisor_cost_factor" MsgModifierPcNegReduced)
        ,("trade_laws_cost_factor"      , numericLoc "trade_laws_cost_factor" MsgModifierPcNegReduced)
        ,("improve_relations_maintain_cost_factor" , numericLoc "MODIFIER_IMPROVE_RELATIONS_MAINTAIN_COST_FACTOR" MsgModifierPcNegReduced)
        ,("party_popularity_stability_factor" , numericLoc "MODIFIER_STABILITY_POPULARITY_FACTOR" MsgModifierPcPosReduced)
        ,("political_power_cost"        , numericLoc "MODIFIER_POLITICAL_POWER_COST" MsgModifierColourNeg)
        ,("political_power_gain"        , numericLoc "MODIFIER_POLITICAL_POWER_GAIN" MsgModifierColourPos)
        ,("political_power_factor"      , numericLoc "MODIFIER_POLITICAL_POWER_FACTOR" MsgModifierPcPosReduced)
        ,("stability_factor"            , numericLoc "MODIFIER_STABILITY_FACTOR" MsgModifierPcPosReduced)
        ,("stability_weekly"            , numericLoc "MODIFIER_STABILITY_WEEKLY" MsgModifierPcPosReduced)
        ,("stability_weekly_factor"     , numericLoc "MODIFIER_STABILITY_WEEKLY_FACTOR" MsgModifierPcPosReduced)
        ,("war_support_factor"          , numericLoc "MODIFIER_WAR_SUPPORT_FACTOR" MsgModifierPcPosReduced)
        ,("war_support_weekly"          , numericLoc "MODIFIER_WAR_SUPPORT_WEEKLY" MsgModifierPcPosReduced)
        ,("war_support_weekly_factor"   , numericLoc "MODIFIER_WAR_SUPPORT_WEEKLY_FACTOR" MsgModifierPcPosReduced)
        ,("drift_defence_factor"        , numericLoc "MODIFIER_DRIFT_DEFENCE_FACTOR" MsgModifierPcPosReduced)
        ,("communism_drift"             , numericLoc "communism_drift" MsgModifierColourPos)
        ,("democratic_drift"            , numericLoc "democratic_drift" MsgModifierColourPos)
        ,("fascism_drift"               , numericLoc "fascism_drift" MsgModifierColourPos)
        ,("neutrality_drift"            , numericLoc "neutrality_drift" MsgModifierColourPos)
        ,("communism_acceptance"        , numericLoc "communism_acceptance" MsgModifierColourPos)
        ,("democratic_acceptance"       , numericLoc "democratic_acceptance" MsgModifierColourPos)
        ,("fascism_acceptance"          , numericLoc "fascism_acceptance" MsgModifierColourPos)
        ,("neutrality_acceptance"       , numericLoc "neutrality_acceptance" MsgModifierColourPos)
            -- Diplomacy
        ,("generate_wargoal_tension"    , numericLoc "MODIFIER_GENERATE_WARGOAL_TENSION_LIMIT" MsgModifierPcReduced) -- yellow
        ,("guarantee_cost"              , numericLoc "MODIFIER_GUARANTEE_COST" MsgModifierPcNegReduced)
        ,("guarantee_tension"           , numericLoc "MODIFIER_GUARANTEE_TENSION_LIMIT" MsgModifierPcNegReduced)
        ,("join_faction_tension"        , numericLoc "MODIFIER_JOIN_FACTION_TENSION_LIMIT" MsgModifierPcNegReduced)
        ,("justify_war_goal_time"       , numericLoc "MODIFIER_JUSTIFY_WAR_GOAL_TIME" MsgModifierPcNegReduced)
        ,("lend_lease_tension"          , numericLoc "MODIFIER_LEND_LEASE_TENSION_LIMIT" MsgModifierPcNegReduced)
        ,("opinion_gain_monthly"        , numericLoc "MODIFIER_OPINION_GAIN_MONTHLY" MsgModifier) -- flat pos
        ,("opinion_gain_monthly_factor" , numericLoc "MODIFIER_OPINION_GAIN_MONTHLY_FACTOR" MsgModifierPcPosReduced)
        ,("opinion_gain_monthly_same_ideology_factor" , numericLoc "MODIFIER_OPINION_GAIN_MONTHLY_SAME_IDEOLOGY_FACTOR" MsgModifierPcPosReduced)
        ,("request_lease_tension"       , numericLoc "MODIFIER_REQUEST_LEASE_TENSION_LIMIT" MsgModifierPcNegReduced)
        ,("surrender_limit"             , numericLoc "MODIFIER_SURRENDER_LIMIT" MsgModifierPcPosReduced)
        ,("send_volunteer_divisions_required" , numericLoc "MODIFIER_SEND_VOLUNTEER_DIVISIONS_REQUIRED" MsgModifierPcNegReduced)
        ,("send_volunteer_size"         , numericLoc "MODIFIER_SEND_VOLUNTEER_SIZE" MsgModifierColourPos)
        ,("send_volunteers_tension"     , numericLoc "MODIFIER_SEND_VOLUNTEERS_TENSION_LIMIT" MsgModifierPcNegReduced)
            -- autonomy
        ,("subjects_autonomy_gain"      , numericLoc "MODIFIER_AUTONOMY_SUBJECT_GAIN" MsgModifierColourPos)
            -- Governments in exile
            -- Equipment
        ,("equipment_conversion_speed"  , numericLoc "EQUIPMENT_CONVERSION_SPEED_MODIFIERS" MsgModifierPcPosReduced)
        ,("equipment_upgrade_xp_cost"   , numericLoc "MODIFIER_EQUIPMENT_UPGRADE_XP_COST" MsgModifier) -- % neg
        ,("license_armor_purchase_cost" , numericLoc "MODIFIER_LICENSE_ARMOR_PURCHASE_COST" MsgModifierPcNegReduced)
        ,("production_factory_efficiency_gain_factor" , numericLoc "MODIFIER_PRODUCTION_FACTORY_EFFICIENCY_GAIN_FACTOR" MsgModifierPcPosReduced)
        ,("production_factory_max_efficiency_factor" , numericLoc "MODIFIER_PRODUCTION_FACTORY_MAX_EFFICIENCY_FACTOR" MsgModifierPcPosReduced)
        ,("production_factory_start_efficiency_factor" , numericLoc "MODIFIER_PRODUCTION_FACTORY_START_EFFICIENCY_FACTOR" MsgModifierPcPosReduced)
            -- Military outside of combat
        ,("command_power_gain"          , numericLoc "MODIFIER_COMMAND_POWER_GAIN" MsgModifierColourPos)
        ,("command_power_gain_mult"     , numericLoc "MODIFIER_COMMAND_POWER_GAIN_MULT" MsgModifierPcPosReduced)
        ,("conscription"                , numericLoc "MODIFIER_CONSCRIPTION_FACTOR" MsgModifierPcReducedSign) --yellow
        ,("conscription_factor"         , numericLoc "MODIFIER_CONSCRIPTION_TOTAL_FACTOR" MsgModifierPcPosReduced)
        ,("training_time_factor"        , numericLoc "MODIFIER_TRAINING_TIME_FACTOR" MsgModifierPcNegReduced)
        ,("max_command_power"           , numericLoc "MODIFIER_MAX_COMMAND_POWER" MsgModifierColourPos)
        ,("max_command_power_mult"      , numericLoc "MODIFIER_MAX_COMMAND_POWER_MULT" MsgModifier)  -- % pos
        ,("training_time_army_factor"   , numericLoc "MODIFIER_TRAINING_TIME_ARMY_FACTOR" MsgModifierPcReducedSign) --yellow
        ,("cat_battlefield_support_cost_factor" , numericLoc "cat_battlefield_support_cost_factor" MsgModifierPcNegReduced)
        ,("unit_light_armor_design_cost_factor" , numericLoc "modifier_unit_light_armor_design_cost_factor" MsgModifierPcNegReduced)
        ,("weekly_manpower"             , numericLoc "MODIFIER_WEEKLY_MANPOWER" MsgModifierColourPos)
            -- Fuel
        ,("base_fuel_gain"              , numericLoc "MODIFIER_BASE_FUEL_GAIN_ADD" MsgModifier) -- flat neutr?
        ,("base_fuel_gain_factor"       , numericLoc "MODIFIER_BASE_FUEL_GAIN_FACTOR" MsgModifier) -- % pos
        ,("fuel_cost"                   , numericLoc "MODIFIER_FUEL_COST" MsgModifier) -- flat neg
        ,("fuel_gain"                   , numericLoc "MODIFIER_FUEL_GAIN_ADD" MsgModifier) -- flat pos
        ,("fuel_gain_factor"            , numericLoc "MODIFIER_MAX_FUEL_FACTOR" MsgModifierPcPosReduced)
        ,("max_fuel"                    , numericLoc "MODIFIER_MAX_FUEL_ADD" MsgModifier) -- flat
        ,("max_fuel_factor"             , numericLoc "MODIFIER_MAX_FUEL_FACTOR" MsgModifierPcPosReduced)
            -- buildings
        ,("civilian_factory_use"        , numericLoc "MODIFIER_CIVILIAN_FACTORY_USE" MsgModifierPc) -- yellow
        ,("industry_free_repair_factor" , numericLoc "MODIFIER_INDUSTRY_FREE_REPAIR_FACTOR" MsgModifierPcPosReduced)
        ,("production_speed_air_base_factor" , numericLoc "modifier_production_speed_air_base_factor" MsgModifierPcPosReduced)
        ,("production_speed_anti_air_building_factor" , numericLoc "modifier_production_speed_anti_air_building_factor" MsgModifierPcPosReduced)
        ,("production_speed_arms_factory_factor" , numericLoc "modifier_production_speed_arms_factory_factor" MsgModifierPcPosReduced)
        ,("production_speed_bunker_factor" , numericLoc "modifier_production_speed_bunker_factor" MsgModifierPcPosReduced)
        ,("production_speed_coastal_bunker_factor" , numericLoc "modifier_production_speed_coastal_bunker_factor" MsgModifierPcPosReduced)
        ,("production_speed_dockyard_factor" , numericLoc "modifier_production_speed_dockyard_factor" MsgModifierPcPosReduced)
        ,("production_speed_infrastructure_factor" , numericLoc "modifier_production_speed_infrastructure_factor" MsgModifierPcPosReduced)
        ,("production_speed_industrial_complex_factor" , numericLoc "modifier_production_speed_industrial_complex_factor" MsgModifierPcPosReduced)
        ,("production_speed_nuclear_reactor_factor" , numericLoc "modifier_production_speed_nuclear_reactor_factor" MsgModifierPcPosReduced)
        ,("production_speed_radar_station_factor" , numericLoc "modifier_production_speed_radar_station_factor" MsgModifierPcPosReduced)
        ,("production_speed_rail_way_factor" , numericLoc "modifier_production_speed_rail_way_factor" MsgModifierPcPosReduced)
        ,("production_speed_rocket_site_factor" , numericLoc "modifier_speed_rocket_site_factor" MsgModifierPcPosReduced)
        ,("production_speed_synthetic_refinery_factor" , numericLoc "modifier_production_speed_synthetic_refinery_factor" MsgModifierPcPosReduced)
        ,("consumer_goods_factor"       , numericLoc "MODIFIER_CONSUMER_GOODS_FACTOR" MsgModifierPcReduced)
        ,("conversion_cost_civ_to_mil_factor" , numericLoc "MODIFIER_CONVERSION_COST_CIV_TO_MIL_FACTOR" MsgModifierPcNegReduced)
        ,("conversion_cost_mil_to_civ_factor" , numericLoc "MODIFIER_CONVERSION_COST_MIL_TO_CIV_FACTOR" MsgModifierPcNegReduced)
        ,("global_building_slots"       , numericLoc "MODIFIER_GLOBAL_BUILDING_SLOTS" MsgModifier) -- flat
        ,("global_building_slots_factor" , numericLoc "MODIFIER_GLOBAL_BUILDING_SLOTS_FACTOR" MsgModifierPcPosReduced)
        ,("industrial_capacity_dockyard" , numericLoc "MODIFIER_INDUSTRIAL_CAPACITY_DOCKYARD_FACTOR" MsgModifierPcPosReduced)
        ,("industrial_capacity_factory" , numericLoc "MODIFIER_INDUSTRIAL_CAPACITY_FACTOR" MsgModifierPcPosReduced)
        ,("industry_repair_factor"      , numericLoc "MODIFIER_INDUSTRY_REPAIR_FACTOR" MsgModifierPcPosReduced)
        ,("line_change_production_efficiency_factor" , numericLoc "MODIFIER_LINE_CHANGE_PRODUCTION_EFFICIENCY_FACTOR" MsgModifierPcPosReduced)
        ,("production_oil_factor"       , numericLoc "MODIFIER_PRODUCTION_OIL_FACTOR" MsgModifierPcPosReduced)
        ,("production_speed_buildings_factor" , numericLoc "MODIFIER_PRODUCTION_SPEED_BUILDINGS_FACTOR" MsgModifierPcPosReduced)
            -- resistance and compliance
        ,("civilian_intel_to_others"    , numericLoc "MODIFIER_CIVILIAN_INTEL_TO_OTHERS" MsgModifierPcNeg)
            -- Intelligence
        ,("foreign_subversive_activites" , numericLoc "MODIFIER_FOREIGN_SUBVERSIVE_ACTIVITIES" MsgModifierPcNegReduced)
            -- Operatives
        ,("enemy_operative_detection_chance_factor" , numericLoc "MODIFIER_ENEMY_OPERATIVE_DETECTION_CHANCE_FACTOR" MsgModifierPcPosReduced)
            -- AI
            -- Unit Leaders
        ,("military_leader_cost_factor" , numericLoc "MODIFIER_MILITARY_LEADER_COST_FACTOR" MsgModifierPcNegReduced)
            -- General Combat
        ,("offence"                     , numericLoc "MODIFIER_OFFENCE" MsgModifierPcPosReduced)
        ,("defence"                     , numericLoc "MODIFIER_DEFENCE" MsgModifierPcPosReduced)
            -- Land Combat
        ,("army_attack_factor"          , numericLoc "MODIFIERS_ARMY_ATTACK_FACTOR" MsgModifierPcPosReduced)
        ,("army_core_attack_factor"     , numericLoc "MODIFIERS_ARMY_CORE_ATTACK_FACTOR" MsgModifierPcPosReduced)
        ,("army_defence_factor"         , numericLoc "MODIFIERS_ARMY_DEFENCE_FACTOR" MsgModifierPcPosReduced)
        ,("army_core_defence_factor"    , numericLoc "MODIFIERS_ARMY_CORE_DEFENCE_FACTOR" MsgModifierPcPosReduced)
        ,("army_morale_factor"          , numericLoc "MODIFIER_ARMY_MORALE_FACTOR" MsgModifierPcPosReduced)
        ,("experience_gain_motorized_combat_factor" , numericLoc "modifier_experience_gain_motorized_combat_factor" MsgModifierPcPosReduced)
        ,("max_dig_in_factor"           , numericLoc "MODIFIER_MAX_DIG_IN_FACTOR" MsgModifierPcPosReduced)
        ,("land_night_attack"           , numericLoc "MODIFIER_LAND_NIGHT_ATTACK" MsgModifierPcPosReduced)
            -- Naval combat
            -- Air combat
            -- targeted
        -- State Scope
        ,("compliance_gain"             , numericLoc "MODIFIER_GLOBAL_NON_CORE_MANPOWER" MsgModifierPcReduced)
        ,("mobilization_speed"          , numericLoc "MODIFIER_MOBILIZATION_SPEED" MsgModifierPcPosReduced)
        ,("non_core_manpower"           , numericLoc "MODIFIER_GLOBAL_NON_CORE_MANPOWER" MsgModifierPcPosReduced)
        ,("resistance_damage_to_garrison" , numericLoc "MODIFIER_RESISTANCE_DAMAGE_TO_GARRISONS" MsgModifierPcNegReduced)
        -- Unit Leader Scope
        ,("trait_panzer_leader_xp_gain_factor" , numericLoc "modifier_trait_panzer_leader_xp_gain_factor" MsgModifierPcPosReduced)
        ]

-- | Handlers for simple compound statements
handlersCompound :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersCompound = Tr.fromList
        -- Note that "any" can mean "all" or "one or more" depending on context.
        [        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>."
        -- trigger scopes
         ("all_allied_country" {- sic -}, scope HOI4Country     . compoundMessage MsgAllAlliedCountry)
        ,("all_army_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllArmyLeader)
        ,("all_character"               , scope HOI4Character   . compoundMessage MsgAllCharacter)
        ,("all_controlled_state"        , scope HOI4ScopeState  . compoundMessage MsgAllControlledState)
        ,("all_core_state"              , scope HOI4ScopeState  . compoundMessage MsgAllCoreState)
        ,("all_country"                 , scope HOI4Country     . compoundMessage MsgAllCountry)
        ,("all_country_with_original_tag", scope HOI4Country    . compoundMessageExtract "original_tag_to_check" MsgAllCountryWithOriginalTag)
        ,("all_enemy_country"           , scope HOI4Country     . compoundMessage MsgAllEnemyCountry)
        ,("all_guaranteed_country"      , scope HOI4Country     . compoundMessage MsgAllGuaranteedCountry)
        ,("all_navy_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllNavyLeader)
        ,("all_neighbor_country"        , scope HOI4Country     . compoundMessage MsgAllNeighborCountry)
        ,("all_neighbor_state"          , scope HOI4ScopeState  . compoundMessage MsgAllNeighborState)
        ,("all_occupied_country"        , scope HOI4Country     . compoundMessage MsgAllOccupiedCountry)
        ,("all_operative_leader"        , scope HOI4Operative   . compoundMessage MsgAllOperativeLeader)
        ,("all_other_country"           , scope HOI4Country     . compoundMessage MsgAllOtherCountry)
        ,("all_owned_state"             , scope HOI4ScopeState  . compoundMessage MsgAllOwnedState)
        ,("all_state"                   , scope HOI4ScopeState  . compoundMessage MsgAllState)
        ,("all_subject_countries"{- sic -}, scope HOI4Country    . compoundMessage MsgAllSubjectCountries)
        ,("all_unit_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllUnitLeader)
        ,("any_allied_country"          , scope HOI4Country     . compoundMessage MsgAnyAlliedCountry)
        ,("any_army_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAnyArmyLeader)
        ,("any_character"               , scope HOI4Character   . compoundMessage MsgAnyCharacter)
        ,("any_controlled_state"        , scope HOI4ScopeState  . compoundMessage MsgAnyControlledState)
        ,("any_core_state"              , scope HOI4ScopeState  . compoundMessage MsgAnyCoreState)
        ,("any_country"                 , scope HOI4Country     . compoundMessage MsgAnyCountry)
        ,("any_country_with_original_tag", scope HOI4Country    . compoundMessageExtract "original_tag_to_check" MsgAnyCountryWithOriginalTag)
        ,("any_enemy_country"           , scope HOI4Country     . compoundMessage MsgAnyEnemyCountry)
        ,("any_guaranteed_country"      , scope HOI4Country     . compoundMessage MsgAnyGuaranteedCountry)
        ,("any_home_area_neighbor_country", scope HOI4Country    . compoundMessage MsgAnyHomeAreaNeighborCountry)
        ,("any_navy_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAnyNavyLeader)
        ,("any_neighbor_country"        , scope HOI4Country     . compoundMessage MsgAnyNeighborCountry)
        ,("any_neighbor_state"          , scope HOI4ScopeState  . compoundMessage MsgAnyNeighborState)
        ,("any_occupied_country"        , scope HOI4Country     . compoundMessage MsgAnyOccupiedCountry)
        ,("any_operative_leader"        , scope HOI4Operative   . compoundMessage MsgAnyOperativeLeader)
        ,("any_other_country"           , scope HOI4Country     . compoundMessage MsgAnyOtherCountry)
        ,("any_owned_state"             , scope HOI4ScopeState  . compoundMessage MsgAnyOwnedState)
        ,("any_state"                   , scope HOI4ScopeState  . compoundMessage MsgAnyState)
        ,("any_subject_country"         , scope HOI4Country     . compoundMessage MsgAnySubjectCountry)
        ,("any_unit_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAnyUnitLeader)
        -- effect scopes
        ,("every_army_leader"           , scope HOI4UnitLeader  . compoundMessage MsgEveryArmyLeader)
        ,("every_character"             , scope HOI4Character   . compoundMessage MsgEveryCharacter)
        ,("every_controlled_state"      , scope HOI4ScopeState  . compoundMessage MsgEveryControlledState)
        ,("every_core_state"            , scope HOI4ScopeState  . compoundMessage MsgEveryCoreState)
        ,("every_country"               , scope HOI4Country     . compoundMessage MsgEveryCountry)
        ,("every_country_with_original_tag", scope HOI4Country  . compoundMessageExtract "original_tag_to_check" MsgEveryCountryWithOriginalTag)
        ,("every_enemy_country"         , scope HOI4Country     . compoundMessage MsgEveryEnemyCountry)
        ,("every_navy_leader"           , scope HOI4UnitLeader  . compoundMessage MsgEveryNavyLeader)
        ,("every_neighbor_country"      , scope HOI4Country     . compoundMessage MsgEveryNeighborCountry)
        ,("every_neighbor_state"        , scope HOI4ScopeState  . compoundMessage MsgEveryNeighborState)
        ,("every_occupied_country"      , scope HOI4Country     . compoundMessage MsgEveryOccupiedCountry)
        ,("every_operative"             , scope HOI4Operative   . compoundMessage MsgEveryOperative)
        ,("every_other_country"         , scope HOI4Country     . compoundMessage MsgEveryOtherCountry)
        ,("every_owned_state"           , scope HOI4ScopeState  . compoundMessage MsgEveryOwnedState)
        ,("every_state"                 , scope HOI4ScopeState  . compoundMessage MsgEveryState)
        ,("every_subject_country"       , scope HOI4Country     . compoundMessage MsgEverySubjectCountry)
        ,("every_unit_leader"           , scope HOI4UnitLeader  . compoundMessage MsgEveryUnitLeader)
        ,("global_every_army_leader"    , scope HOI4UnitLeader  . compoundMessage MsgGlobalEveryArmyLeader)
        ,("random_army_leader"          , scope HOI4UnitLeader  . compoundMessage MsgRandomArmyLeader)
        ,("random_character"            , scope HOI4Character   . compoundMessage MsgRandomCharacter)
        ,("random_controlled_state"     , scope HOI4ScopeState  . compoundMessage MsgRandomControlledState)
        ,("random_core_state"           , scope HOI4ScopeState  . compoundMessage MsgRandomCoreState)
        ,("random_country"              , scope HOI4Country     . compoundMessage MsgRandomCountry)
        ,("random_country_with_original_tag", scope HOI4Country . compoundMessageExtract "original_tag_to_check" MsgRandomCountryWithOriginalTag)
        ,("random_enemy_country"        , scope HOI4Country     . compoundMessage MsgRandomEnemyCountry)
        ,("random_navy_leader"          , scope HOI4UnitLeader  . compoundMessage MsgRandomNavyLeader)
        ,("random_neighbor_country"     , scope HOI4Country     . compoundMessage MsgRandomNeighborCountry)
        ,("random_neighbor_state"       , scope HOI4ScopeState  . compoundMessage MsgRandomNeighborState)
        ,("random_occupied_country"     , scope HOI4Country     . compoundMessage MsgRandomOccupiedCountry)
        ,("random_operative"            , scope HOI4Operative   . compoundMessage MsgRandomOperative)
        ,("random_other_country"        , scope HOI4Country     . compoundMessage MsgRandomOtherCountry)
        ,("random_owned_controlled_state", scope HOI4Country     . compoundMessage MsgRandomOwnedControlledState)
        ,("random_owned_state"          , scope HOI4ScopeState  . compoundMessage MsgRandomOwnedState)
        ,("random_state"                , scope HOI4ScopeState  . compoundMessage MsgRandomState)
        ,("random_subject_country"      , scope HOI4Country     . compoundMessage MsgRandomSubjectCountry)
        ,("random_unit_leader"          , scope HOI4UnitLeader  . compoundMessage MsgRandomUnitLeader)
        -- dual scopes
        ,("root"                        , compoundMessagePronoun) --ROOT
        ,("prev"                        , compoundMessagePronoun) --PREV
        ,("from"                        , compoundMessagePronoun) --FROM
        -- no THIS, not used on LHS
        ,("overlord"                    , scope HOI4Country   . compoundMessage MsgOverlord)
        ,("owner"                       , scope HOI4Country   . compoundMessage MsgOwner)
        ,("controller"                  , scope HOI4Country   . compoundMessage MsgController)
        ,("capital_scope"               , scope HOI4ScopeState  . compoundMessage MsgCapital)
        ,("event_target"        , compoundMessageTagged MsgSCOPEEventTarget (Just HOI4From)) -- Tagged blocks
        ,("var"                 , compoundMessageTagged MsgSCOPEVariable (Just HOI4From)) -- Tagged blocks
        -- flow control
        ,("and"                         , compoundMessage MsgAnd) --AND
        ,("not"                         , compoundMessage MsgNot) --NOT
        ,("or"                          , compoundMessage MsgOr) --OR
        ,("count_triggers"          ,                      compoundMessage MsgCountTriggers)
        ,("hidden_trigger"          ,                      compoundMessage MsgHiddenTriggers)
        ,("custom_trigger_tooltip"  ,                      compoundMessage MsgCustomTriggerTooltip)
        ,("hidden_effect"           ,                      compoundMessage MsgHiddenEffect)
        ,("else"                    ,                      compoundMessage MsgElse)
        ,("else_if"                 ,                      compoundMessage MsgElseIf)
        ,("if"                      ,                      compoundMessage MsgIf) -- always needs editing
        ,("limit"                   , setIsInEffect False . compoundMessage MsgLimit) -- always needs editing
        ,("prioritize"              ,                       prioritize) -- always needs editing
        ,("while_loop_effect"       ,                       compoundMessage MsgWhile) -- always needs editing
        ,("for_loop_effect"         ,                       compoundMessage MsgFor) -- always needs editing
        -- random and random_list are also part of flow control but are more complicated
        ]

withLocAtomName :: (HOI4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withLocAtomName msg = withLocAtom' msg (<> "_name")

-- | Handlers for simple statements where RHS is a localizable atom
handlersLocRhs :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersLocRhs = Tr.fromList
        [("create_faction"        , withLocAtom MsgCreateFaction)
        ,("set_state_name"        , withLocAtom MsgSetStateName)
        ,("set_state_category"    , withLocAtom MsgSetStateCategory)
        ,("custom_effect_tooltip" , withLocAtom MsgCustomEffectTooltip)
        ,("has_opinion_modifier"  , withLocAtom MsgHasOpinionMod)
        ,("has_tech"              , withLocAtom MsgHasTech)
        ,("is_character"          , withLocAtom MsgIsCharacter)
        ,("is_on_continent"       , withLocAtom MsgIsOnContinent)
        ,("is_in_tech_sharing_group" , withLocAtomName MsgIsInTechSharingGroup)
        ,("add_to_tech_sharing_group" , withLocAtomName MsgAddToTechSharingGroup)
        ,("tooltip"               , withLocAtom MsgTooltip)
        ,("unlock_decision_category_tooltip" , withLocAtom MsgUnlockDecisionCategoryTooltip)
        ,("unlock_decision_tooltip" , withLocAtom MsgUnlockDecisionTooltip)
        ]

-- | Handlers for statements whose RHS is a state ID
handlersState :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersState = Tr.fromList
        [("add_state_claim"     , withState MsgAddStateClaim)
        ,("add_state_core"      , withState MsgAddStateCore)
        ,("controls_state"      , withState MsgControlsState)
        ,("has_full_control_of_state" , withState MsgHasFullControlOfState)
        ,("owns_state"          , withState MsgOwnsState)
        ,("remove_state_claim"  , withState MsgRemoveStateClaim)
        ,("remove_state_core"   , withState MsgRemoveStateCore)
        ,("set_capital"         , withState MsgSetCapital)
        ,("transfer_state"      , withState MsgTransferState)
        ]

-- | Handlers for statements whose RHS is a flag OR a province ID
-- Also abusable for tag,scope purposes
handlersFlagOrstate :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrstate = Tr.fromList
        [
        ]

-- | Handlers for statements whose RHS is a number OR a tag/pronoun, with icon
handlersNumericOrFlag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericOrFlag = Tr.fromList
        [
        ]

-- TODO: parse advisor files
-- | Handlers for statements whose RHS is an advisor ID
handlersAdvisorId :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersAdvisorId = Tr.fromList
        [
        ]

-- | Simple statements whose RHS should be presented as is, in typewriter face
handlersTypewriter :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTypewriter = Tr.fromList
        [("clr_character_flag"  , withNonlocAtom2 MsgCharacterFlag MsgClearFlag)
        ,("clr_country_flag"    , withNonlocAtom2 MsgCountryFlag MsgClearFlag)
        ,("clr_global_flag"     , withNonlocAtom2 MsgGlobalFlag MsgClearFlag)
        ,("clr_state_flag"      , withNonlocAtom2 MsgStateFlag MsgClearFlag)
        ,("clr_unit_leader_flag" , withNonlocAtom2 MsgUnitLeaderFlag MsgClearFlag)
        ,("has_focus_tree"      , withNonlocAtom MsgHasFocusTree)
        ,("save_event_target_as", withNonlocAtom MsgSaveEventTargetAs)
        ,("save_global_event_target_as", withNonlocAtom MsgSaveGlobalEventTargetAs)
        ,("set_cosmetic_tag"    , withNonlocAtom MsgSetCosmeticTag)
        ]

-- | Handlers for simple statements with icon
handlersSimpleIcon :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleIcon = Tr.fromList
        [("has_autonomy_state"      , withLocAtomIcon MsgHasAutonomyState)
        ,("has_government"          , withLocAtomIcon MsgHasGovernment)
        ]

-- | Handlers for simple statements with a flag or pronoun
handlersSimpleFlag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleFlag = Tr.fromList
        [("add_claim_by"            , withFlag MsgAddClaimBy)
        ,("add_core_of"             , withFlag MsgAddCoreOf)
        ,("add_to_faction"          , withFlag MsgAddToFaction)
        ,("change_tag_from"         , withFlag MsgChangeTagFrom)
        ,("is_controlled_by"        , withFlag MsgIsControlledBy)
        ,("country_exists"          , withFlag MsgCountryExists)
        ,("has_defensive_war_with"  , withFlag MsgHasDefensiveWarWith)
        ,("give_guarantee"          , withFlag MsgGiveGuarantee)
        ,("give_military_access"    , withFlag MsgGiveMilitaryAccess)
        ,("has_guaranteed"          , withFlag MsgHasGuaranteed)
        ,("has_non_aggression_pact_with" , withFlag MsgHasNonAggressionPactWith)
        ,("has_offensive_war_with"  , withFlag MsgHasOffensiveWarWith)
        ,("has_subject"             , withFlag MsgHasSubject)
        ,("inherit_technology"      , withFlag MsgInheritTechnology)
        ,("is_fully_controlled_by"  , withFlag MsgIsFullyControlledBy)
        ,("is_guaranteed_by"        , withFlag MsgIsGuaranteedBy)
        ,("is_in_faction_with"      , withFlag MsgIsInFactionWith)
        ,("is_justifying_wargoal_against" , withFlag MsgIsJustifyingWargoalAgainst)
        ,("is_neighbor_of"          , withFlag MsgNeighbors)
        ,("is_owned_and_controlled_by" , withFlag MsgIsOwnedAndControlledBy)
        ,("is_puppet_of"            , withFlag MsgIsPuppetOf)
        ,("is_core_of"              , withFlag MsgIsStateCore)
        ,("is_subject_of"           , withFlag MsgIsSubjectOf)
        ,("is_owned_by"             , withFlag MsgIsOwnedBy)
        ,("remove_claim_by"          , withFlag MsgRemoveClaimBy)
        ,("remove_core_of"          , withFlag MsgRemoveCoreOf)
        ,("remove_from_faction"     , withFlag MsgRemoveFromFaction)
        ,("tag"                     , withFlag MsgCountryIs)
        ,("has_war_with"            , withFlag MsgHasWarWith)
        ,("has_war_together_with"   , withFlag MsgHasWarTogetherWith)
        ,("original_tag"            , withFlag MsgOrignalTag)
        ,("white_peace"             , withFlag MsgMakeWhitePeace)
        ]

-- | Handlers for simple generic statements with a flag or "yes"/"no"
handlersFlagOrYesNo :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrYesNo = Tr.fromList
        [
        ]

-- | Handlers for statements whose RHS may be an icon, a flag, a province, or a
-- pronoun (such as ROOT).
handlersIconFlagOrPronoun :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIconFlagOrPronoun = Tr.fromList
        [
        ]



-- | Handlers for yes/no statements
handlersYesNo :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersYesNo = Tr.fromList
        [("is_ai"                       , withBool MsgIsAIControlled)
        ,("always"                      , withBool MsgAlways)
        ,("exists"                      , withBool MsgExists)
        ,("has_capitulated"             , withBool MsgHasCapitulated)
        ,("has_civil_war"               , withBool MsgHasCivilWar)
        ,("has_defensive_war"           , withBool MsgHasDefensiveWar)
        ,("has_offensive_war"           , withBool MsgHasOffensiveWar)
        ,("has_war"                     , withBool MsgHasWar)
        ,("is_capital"                  , withBool MsgIsCapital)
        ,("is_coastal"                  , withBool MsgIsCoastal)
        ,("is_demilitarized_zone"       , withBool MsgIsDemilitarizedZone)
        ,("is_faction_leader"           , withBool MsgIsFactionLeader)
        ,("is_female"                   , withBool MsgIsFemale)
        ,("is_historical_focus_on"      , withBool MsgIsHistoricalFocusOn)
        ,("is_in_faction"               , withBool MsgIsInFaction)
        ,("is_in_home_area"             , withBool MsgIsInHomeArea)
        ,("is_island_state"             , withBool MsgIsIslandState)
        ,("is_major"                    , withBool MsgIsMajor)
        ,("is_puppet"                   , withBool MsgIsPuppet)
        ,("is_subject"                  , withBool MsgIsSubject)
        ,("is_unit_leader"              , withBool MsgIsUnitLeader)
        ,("set_demilitarized_zone"      , withBool MsgSetDemilitarizedZone)
        ]

-- | Handlers for statements that may be numeric or a tag
handlersNumericOrTag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericOrTag = Tr.fromList
        [
        ]

-- | Handlers querying the number of provinces of some kind, mostly religions
-- and trade goods
handlersNumStates :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumStates = Tr.fromList
        [
        ]

-- Helpers for text/value pairs
tryLocAndIconTitle :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Text, Text)
tryLocAndIconTitle t = tryLocAndIcon (t <> "_title")

-- | Handlers for text/value pairs.
--
-- $textvalue
handlersTextValue :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextValue = Tr.fromList
        [("add_offsite_building"        , textValue "type" "level" MsgAddOffsiteBuilding  MsgAddOffsiteBuilding tryLocAndIcon)
        ,("add_popularity"              , textValue "ideology" "popularity" MsgAddPopularity MsgAddPopularity tryLocAndIcon)
        ,("has_volunteers_amount_from"  , textValueCompare "tag" "count" "more than" "less than" MsgHasVolunteersAmountFrom MsgHasVolunteersAmountFrom flagTextMaybe)
        ,("modify_tech_sharing_bonus"   , textValue "id" "bonus" MsgModifyTechSharingBonus MsgModifyTechSharingBonus tryLocMaybe) --icon ignored
        ,("set_province_name"           , textValue "name" "id" MsgSetProvinceName MsgSetProvinceName tryLocMaybe)
        ,("set_victory_points"          , valueValue "province" "value" MsgSetVictoryPoints MsgSetVictoryPoints)
        ,("strength_ratio"              , textValueCompare "tag" "ratio" "more than" "less than" MsgStrengthRatio MsgStrengthRatio flagTextMaybe)
        ,("remove_building"             , textValue "type" "level" MsgRemoveBuilding MsgRemoveBuilding tryLocAndIcon)
        ,("modify_character_flag"       , withNonlocTextValue "flag" "value" MsgCharacterFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_country_flag"         , withNonlocTextValue "flag" "value" MsgCountryFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_global_flag"          , withNonlocTextValue "flag" "value" MsgGlobalFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_state_flag"           , withNonlocTextValue "flag" "value" MsgStateFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_unit_leader_flag"     , withNonlocTextValue "flag" "value" MsgUnitLeaderFlag MsgModifyFlag) -- Localization/icon ignored
        ]

-- | Handlers for text/atom pairs
handlersTextAtom :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextAtom = Tr.fromList
        [("has_game_rule"               , textAtom "rule" "option" MsgHasRule tryLoc)
        ]

-- | Handlers for special complex statements
handlersSpecialComplex :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSpecialComplex = Tr.fromList
        [("add_building_construction"    , addBuildingConstruction)
        ,("add_doctrine_cost_reduction"  , addDoctrineCostReduction)
        ,("add_named_threat"             , addNamedThreat)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion MsgAddOpinionDur)
        ,("add_tech_bonus"               , addTechBonus)
        ,("add_to_war"                   , addToWar)
        ,("annex_country"                , annexCountry)
        ,("reverse_add_opinion_modifier" , opinion MsgReverseAddOpinion MsgReverseAddOpinionDur)
        ,("build_railway"                , buildRailway)
        ,("can_build_railway"            , canBuildRailway)
        ,("create_equipment_variant"     , createEquipmentVariant)
        ,("create_wargoal"               , createWargoal)
        ,("custom_trigger_tooltip"       , customTriggerTooltip)
        ,("country_event"                , scope HOI4Country . triggerEvent MsgCountryEvent)
        ,("declare_war_on"               , declareWarOn)
        ,("free_building_slots"          , freeBuildingSlots)
        ,("has_completed_focus"          , handleFocus MsgHasCompletedFocus)
        ,("complete_national_focus"      , handleFocus MsgCompleteNationalFocus)
        ,("focus"                        , handleFocus MsgFocus) -- used in pre-requisite for focuses
        ,("focus_progress"               , focusProgress MsgFocusProgress)
        ,("has_army_size"                , hasArmySize)
        ,("has_opinion"                  , hasOpinion MsgHasOpinion)
        ,("has_country_leader"           , hasCountryLeader)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion (\modid what who _years -> MsgAddOpinion modid what who))
        ,("load_focus_tree"              , loadFocusTree)
        ,("modify_building_resources"    , modifyBuildingResources)
        ,("news_event"                   , scope HOI4Country . triggerEvent MsgNewsEvent)
        ,("remove_opinion_modifier"      , opinion MsgRemoveOpinionMod (\modid what who _years -> MsgRemoveOpinionMod modid what who))
        ,("set_autonomy"                 , setAutonomy)
        ,("set_politics"                 , setPolitics)
        ,("set_party_name"               , setPartyName)
        ,("start_civil_war"              , startCivilWar)
        ,("state_event"                  , scope HOI4ScopeState . triggerEvent MsgStateEvent)
        ,("unit_leader_event"            , scope HOI4UnitLeader . triggerEvent MsgUnitLeaderEvent)
        ,("operative_leader_event"       , scope HOI4Operative . triggerEvent MsgOperativeEvent)
        -- flags
        ,("set_character_flag"           , setFlag MsgCharacterFlag)
        ,("set_country_flag"             , setFlag MsgCountryFlag)
        ,("set_global_flag"              , setFlag MsgGlobalFlag)
        ,("set_state_flag"               , setFlag MsgStateFlag)
        ,("set_unit_leader_flag"         , setFlag MsgUnitLeaderFlag)
        ,("has_character_flag"           , hasFlag MsgCharacterFlag)
        ,("has_country_flag"             , hasFlag MsgCountryFlag)
        ,("has_global_flag"              , hasFlag MsgGlobalFlag)
        ,("has_state_flag"               , hasFlag MsgStateFlag)
        ,("has_unit_leader_flag"         , hasFlag MsgUnitLeaderFlag)

        ,("set_nationality"              , setNationality)

        -- Effects
        -- simpleEffectAtom and simpleEffectNum

        -- Variables
        ,("set_variable"                 , setVariable MsgSetVariable MsgSetVariableVal)
        ,("set_temp_variable"            , setVariable MsgSetTempVariable MsgSetTempVariableVal)
        ,("add_to_variable"              , setVariable MsgAddVariable MsgAddVariableVal)
        ,("add_to_temp_variable"         , setVariable MsgAddTempVariable MsgAddTempVariableVal)
        ,("subtract_variable"            , setVariable MsgSubVariable MsgSubVariableVal)
        ,("subtract_temp_variable"       , setVariable MsgSubTempVariable MsgSubTempVariableVal)
        ,("multiply_variable"            , setVariable MsgMulVariable MsgMulVariableVal)
        ,("multiply_temp_variable"       , setVariable MsgMulTempVariable MsgMulTempVariableVal)
        ,("divide_variable"              , setVariable MsgDivVariable MsgDivVariableVal)
        ,("divide_temp_variable"         , setVariable MsgDivTempVariable MsgDivTempVariableVal)
        ,("check_variable"               , setVariable MsgChkVariable MsgChkVariableVal)
        ,("is_variable_equal"            , setVariable MsgEquVariable MsgEquVariableVal)
        ,("export_to_variable"           , exportVariable)
        ]

-- | Handlers for idea groups
handlersIdeas :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIdeas = Tr.fromList
        [("has_idea"                     , handleIdeas False MsgHasIdea)
        ,("add_ideas"                    , handleIdeas True MsgAddIdea)
        ,("remove_ideas"                 , handleIdeas False MsgRemoveIdea)
        ,("add_timed_idea"               , handleTimedIdeas MsgAddTimedIdea)
        ,("modify_timed_idea"            , handleTimedIdeas MsgModifyTimedIdea)
        ,("swap_ideas"                   , handleSwapIdeas)
        ]

-- | Handlers for miscellaneous statements
handlersMisc :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersMisc = Tr.fromList
        [("add_autonomy_ratio"          , rhsIgnored MsgAddAiStrategy)
        ,("add_autonomy_ratio"          , addAutonomyRatio)
        ,("add_resource"                , addResource)
        ,("random"                      , random)
        ,("random_list"                 , randomList)
        -- Special
        ,("add_estate_loyalty_modifier" , addEstateLoyaltyModifier)
--        ,("add_manpower"        , gainMen)
--        ,("add_sailors"         , gainMen)
        ,("calc_true_if"        , calcTrueIf)
        ,("change_estate_land_share" , changeEstateLandShare)
        ,("create_independent_estate" , createIndependentEstate)
        ,("create_subject"      , taTypeFlag "subject_type" "subject" MsgCreateSubject)
        ,("change_subject_type" , simpleEffectAtom "subject_type" MsgChangeSubjectType)
        ,("create_succession_crisis" , createSuccessionCrisis)
        ,("date"                , dateHandle)
        ,("development_in_provinces" , numOwnedProvincesWith MsgDevelopmentInProvinces)
        ,("dominant_culture"    , dominantCulture)
        ,("diplomatic_relation" , diplomaticRelation)
        ,("dynasty"             , dynasty)
        ,("government_rank"     , govtRank)
        ,("has_dlc"             , hasDlc)
        ,("has_equipment"       , hasEquipment)
        ,("has_government_attribute" , hasGovermentAttribute)
        ,("has_heir"            , hasHeir)
        ,("has_wargoal_against" , hasWarGoalAgainst)
        ,("is_month"            , isMonth)
        ,("kill_leader"         , killLeader)
        ,("kill_heir"           , killHeir)
        ,("kill_units"          , killUnits)
        ,("num_of_owned_provinces_with" , numOwnedProvincesWith MsgNumOwnedProvincesWith)
        ,("num_of_provinces_owned_or_owned_by_non_sovereign_subjects_with" , numOwnedProvincesWith MsgNumOwnedProvincesOrNonSovereignSubjectsWith)
        ,("num_of_religion"     , numOfReligion)
        ,("num_of_states_owned_or_owned_by_non_sovereign_subjects_with" , numOwnedProvincesWith MsgNumOwnedStatesOrNonSovereignSubjectsWith)
        ,("piety"               , piety)
        ,("production_leader"   , productionLeader)
        ,("range"               , range)
        ,("remove_casus_belli"  , taTypeFlag "type" "target" MsgRemoveCasusBelli)
        ,("remove_trade_modifier" , taTypeFlag "name" "who" MsgRemoveTradeModifier)
        ,("send_equipment"      , sendEquipment)
        ,("set_government_rank" , setGovtRank)
        ,("set_rule"            , setRule MsgSetRule)
        ,("set_saved_name"      , setSavedName)
        ,("generate_advisor_of_type_and_semi_random_religion_effect" , randomAdvisor)
--        ,("create_colony_mission_reward" , createColonyMissionReward)
--        ,("has_idea_group"      , hasIdeaGroup)
        -- Estate monopoly privileges
        ,("apply_estate_monopoly_modifier"  , estatePrivilege MsgApplyEstateModifer)
        ,("reapply_estate_privilege"        , estatePrivilege MsgReapplyEstatePrivilege)
        ,("remove_estate_monopoly_modifier" , estatePrivilege MsgRemoveEstateModifer)
        -- Estate land share effects (from 01_scripted_effects_for_estates.txt)
        ,("give_estate_land_share_small"    , estateLandShareEffect 1)
        ,("give_estate_land_share_medium"   , estateLandShareEffect 2)
        ,("give_estate_land_share_large"    , estateLandShareEffect 3)
        ,("give_estate_land_share_massive"  , estateLandShareEffect 5)
        ,("give_estate_land_share_gigantic" , estateLandShareEffect 10)
        ,("take_estate_land_share_small"    , estateLandShareEffect (-1))
        ,("take_estate_land_share_medium"   , estateLandShareEffect (-2))
        ,("take_estate_land_share_large"    , estateLandShareEffect (-3))
        ,("take_estate_land_share_massive"  , estateLandShareEffect (-5))
        ,("take_estate_land_share_gigantic" , estateLandShareEffect (-10))
        -- Conditions that require a certain number of buildings
        ,("cathedral"                        , buildingCount)
        ,("counting_house"                   , buildingCount)
        ,("farm_estate"                      , buildingCount)
        ,("fort_15th"                        , buildingCount)
        ,("furnace"                          , buildingCount)
        ,("grand_shipyard"                   , buildingCount)
        ,("marketplace"                      , buildingCount)
        ,("mills"                            , buildingCount)
        ,("native_ceremonial_fire_pit"       , buildingCount)
        ,("native_longhouse"                 , buildingCount)
        ,("native_sweat_lodge"               , buildingCount)
        ,("plantations"                      , buildingCount)
        ,("shipyard"                         , buildingCount)
        ,("stock_exchange"                   , buildingCount)
        ,("temple"                           , buildingCount)
        ,("textile"                          , buildingCount)
        ,("trade_depot"                      , buildingCount)
        ,("university"                       , buildingCount)
        ,("wharf"                            , buildingCount)
        ,("workshop"                         , buildingCount)
        -- Institutions
        ,("feudalism"                        , institutionPresence)
        ,("renaissance"                      , institutionPresence)
        ,("new_world_i"                      , institutionPresence)
        ,("printing_press"                   , institutionPresence)
        ,("global_trade"                     , institutionPresence)
        ,("manufactories"                    , institutionPresence)
        ,("enlightenment"                    , institutionPresence)
        ,("industrialization"                , institutionPresence)
        ]

-- | Handlers for ignored statements
handlersIgnored :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIgnored = Tr.fromList
        [("custom_tooltip", return $ return [])
        ,("effect_tooltip", return $ return []) -- shows the effects but doesn't execute them, don't know if I want it to show up in the parser
        ,("goto"          , return $ return [])
        ,("log"           , return $ return [])
        ,("original_tag_to_check" , return $ return [])
        ,("required_personality", return $ return[]) -- From the 1.30 patch notes: "The required_personality field will now be ignored"
        ,("highlight"     , return $ return [])
        ,("picture"       , return $ return []) -- Some modifiers have custom pictures
        ]

-- | Extract the appropriate message(s) from a single statement. Note that this
-- may produce many lines (via 'ppMany'), since some statements are compound.
ppOne :: (HOI4Info g, Monad m) => StatementHandler g m
ppOne stmt@[pdx| %lhs = %rhs |] = ppOne' stmt lhs rhs
ppOne stmt@[pdx| %lhs > %rhs |] = ppOne' stmt lhs rhs
ppOne stmt@[pdx| %lhs < %rhs |] = ppOne' stmt lhs rhs
ppOne stmt = preStatement stmt
ppOne' :: (HOI4Info g, Monad m) =>
    GenericStatement
    -> Lhs lhs
    -> Rhs Void Void
    -> PPT g m IndentedMessages
ppOne' stmt lhs rhs = case lhs of
    GenericLhs label _ -> case Tr.lookup (TE.encodeUtf8 (T.toLower label)) ppHandlers of
        Just handler -> handler stmt
        -- default
        Nothing -> if isTag label
             then case rhs of
                CompoundRhs scr ->
                    withCurrentIndent $ \_ -> do -- force indent level at least 1
                        lflag <- plainMsg' . (<> ":") =<< flagText (Just HOI4Country) label
                        scriptMsgs <- scope HOI4Country $ ppMany scr
                        return (lflag : scriptMsgs)
                _ -> preStatement stmt
             else do
                geoData <- getGeoData
                mloc <- getGameL10nIfPresent label
                case mloc of
                    -- Check for localizable atoms, e.g. regions
                    Just loc -> case rhs of
                        CompoundRhs scr -> ppMaybeGeo label loc scr
                        _ -> compound loc stmt
                    Nothing -> preStatement stmt
    AtLhs _ -> return [] -- don't know how to handle these
    IntLhs n -> do -- Treat as a province tag
        tradeNodes <- getTradeNodes
        case rhs of
            CompoundRhs scr ->
                -- Check if this is the main province of a trade node in which case we need
                -- to see if that needs to be displayed instead
                case HM.lookup n tradeNodes of
                    Just node | isTradeNodeQuery scr -> do
                        nodeLoc <- getGameL10n node
                        header <- msgToPP (MsgTradeNode nodeLoc)
                        scriptMsgs <- scope HOI4TradeNode $ ppMany scr
                        return (header ++ scriptMsgs)
                    _ -> do
                        state_loc <- getStateLoc n
                        header <- msgToPP (MsgState state_loc)
                        scriptMsgs <- scope HOI4ScopeState $ ppMany scr
                        return (header ++ scriptMsgs)
            _ -> preStatement stmt
    CustomLhs _ -> preStatement stmt

isTradeNodeQuery :: GenericScript -> Bool
isTradeNodeQuery scr = any isTradeNodeQuery' scr
    where
        isTradeNodeQuery' :: GenericStatement -> Bool
        isTradeNodeQuery' stmt@[pdx| $lhs = @scr |] = case Tr.lookup (TE.encodeUtf8 (T.toLower lhs)) isTradeNodeScrTrie of
            Just True -> True
            Just False -> isTradeNodeQuery scr
            _ -> False
        isTradeNodeQuery' stmt@[pdx| $lhs = %_ |] = isJust $ Tr.lookup (TE.encodeUtf8 (T.toLower lhs)) isTradeNodeQueryTrie
        isTradeNodeQuery' _ = False
        -- Conditions/commands that are listed as Province (Trade node) on the wiki
        isTradeNodeQueryTrie :: Trie ()
        isTradeNodeQueryTrie = Tr.fromList
            [("add_trade_node_income",())
            ,("has_merchant",())
            ,("has_most_province_trade_power",())
            ,("has_trader",())
            ,("highest_value_trade_node",())
            ,("is_strongest_trade_power",())
            ,("recall_merchant",())
            ,("trade_range",())
            ]
        isTradeNodeScrTrie :: Trie Bool
        isTradeNodeScrTrie = Tr.fromList
            [("add_trade_modifier"                , True)
            ,("has_privateer_share_in_trade_node" , True)
            ,("has_trade_modifier"                , True)
            ,("most_province_trade_power"         , True)
            ,("privateer_power"                   , True)
            ,("strongest_trade_power"             , True)
            ,("remove_trade_modifier"             , True)
            ,("trade_share"                       , True)

            ,("and"                               , False)
            ,("else"                              , False)
            ,("if"                                , False)
            ,("not"                               , False)
            ,("or"                                , False)
            ]

ppMaybeGeo :: (HOI4Info g, Monad m) => Text -> Text -> GenericScript -> PPT g m IndentedMessages
ppMaybeGeo label loc scr = do
    geoData <- getGeoData
    let (mtypeStmt, rest) = extractStmt (matchExactText "type" "all") scr
    case HM.lookup label geoData of
        Just geoType -> do
            inEffect <- getIsInEffect
            header <- plainMsg' $ (if isJust mtypeStmt || inEffect then "All provinces" else "Any province")
                <> " in the " <> loc <> " " <> (describe geoType) <> ":"
            scriptMsgs <- scope HOI4ScopeState $ ppMany rest
            return (header : scriptMsgs)
        Nothing -> do
            let actScope = if (T.toLower label) `elem` ["emperor", "revolution_target", "crusade_target"] then
                    HOI4Country
                else
                    HOI4ScopeState
            header <- plainMsg' $ loc <> ":"
            scriptMsgs <- scope actScope $ ppMany scr
            return (header : scriptMsgs)

    where
        describe :: HOI4GeoType -> Text
        describe HOI4GeoArea = "area"
        describe HOI4GeoRegion = "region"
        describe HOI4GeoSuperRegion = "subcontinent"
        describe HOI4GeoContinent = "continent"
        describe HOI4GeoTradeCompany = "trade company region"
        describe HOI4GeoColonialRegion = "region" -- No need to say "colonial region" since that's implied by the localized name

-- | Try to extract one matching statement
extractStmt :: (a -> Bool) -> [a] -> (Maybe a, [a])
extractStmt p xs = extractStmt' p xs []
    where
        extractStmt' _ [] acc = (Nothing, acc)
        extractStmt' p (x:xs) acc =
            if p x then
                (Just x, acc++xs)
            else
                extractStmt' p xs (acc++[x])

-- | Predicate for matching text on the left hand side
matchLhsText :: Text -> GenericStatement -> Bool
matchLhsText t s@[pdx| $lhs = %_ |] | t == lhs = True
matchLhsText t s@[pdx| $lhs < %_ |] | t == lhs = True
matchLhsText t s@[pdx| $lhs > %_ |] | t == lhs = True
matchLhsText _ _ = False

-- | Predicate for matching text on boths sides
matchExactText :: Text -> Text -> GenericStatement -> Bool
matchExactText l r s@[pdx| $lhs = $rhs |] | l == lhs && r == T.toLower rhs = True
matchExactText _ _ _ = False
