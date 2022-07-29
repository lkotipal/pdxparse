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
flagTextMaybe = fmap (\t -> (mempty, t)) . flagText (Just HOI4Country)

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
    , handlersSignedNumeric
    , handlersNumStates
    , handlersTextValue
    , handlersTextAtom
    , handlersSpecialComplex
    , handlersRebels
    , handlersIdeaGroups
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

-- | Handlers for statements pertaining to modifiers
handlersModifiers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersModifiers = Tr.fromList
        [("add_dynamic_modifier"           , addDynamicModifier)
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

withLocAtomName msg = withLocAtom' msg (\t -> t <> "_name")

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
        [("add_claim"          , withFlagOrState MsgAddClaimFor MsgAddClaimOn)
--        ,("add_core"           , withFlagOrState MsgGainCore MsgGainCoreProvince)
        ,("add_permanent_claim", withFlagOrState MsgGainPermanentClaimCountry MsgGainPermanentClaimProvince)
        ,("cavalry"            , withFlagOrState MsgCavalrySpawnsCountry MsgCavalrySpawnsProvince)
        ,("infantry"           , withFlagOrState MsgInfantrySpawnsCountry MsgInfantrySpawnsProvince)
        ,("remove_core"        , withFlagOrState MsgLoseCoreCountry MsgLoseCoreProvince)
        ,("is_colonial_nation_of" , withFlagOrState MsgIsColonialNationOf MsgIsColonialNationOf)
        ]

-- | Handlers for statements whose RHS is a number OR a tag/pronoun, with icon
handlersNumericOrFlag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericOrFlag = Tr.fromList
        [("adm_tech"             , withTagOrNumber "adm tech" MsgADMTech MsgADMTechAs)
        ,("army_size"            , withTagOrNumber "" MsgArmySize MsgArmySizeMatches) -- FIXME: Don't really need icon
        ,("development"          , withTagOrNumber "development" MsgDevelopment MsgDevelopmentAs) -- Can't really be a tag, but it can be "CAPITAL"
        ,("land_morale"          , withTagOrNumber "morale of armies" MsgMoraleOfArmies MsgMoraleOfArmiesAs)
        ,("monthly_income"       , withTagOrNumber "ducats" MsgMonthlyIncome MsgMonthlyIncomeAs)
        ,("navy_size"            , withTagOrNumber "" MsgNavySize MsgNavySizeMatches) -- FIXME: Don't really need icon
        ,("num_of_light_ship"    , withTagOrNumber "light ship" MsgNumLightShips MsgNumLightShipsMatches)
        ,("num_of_heavy_ship"    , withTagOrNumber "heavy ship" MsgNumHeavyShips MsgNumHeavyShipsMatches)
        ,("num_of_galley"        , withTagOrNumber "galley" MsgNumGalleyShips MsgNumGalleyShipsMatches)
        ,("num_of_transport"     , withTagOrNumber "transport" MsgNumTransportShips MsgNumTransportShipsMatches)
        ]

-- TODO: parse advisor files
-- | Handlers for statements whose RHS is an advisor ID
handlersAdvisorId :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersAdvisorId = Tr.fromList
        [("advisor_exists"     , numeric MsgAdvisorExists)
        ,("is_advisor_employed", numeric MsgAdvisorIsEmployed)
        ]

-- | Simple statements whose RHS should be presented as is, in typewriter face
handlersTypewriter :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTypewriter = Tr.fromList
        [("clr_character_flag"  , withNonlocAtom2 MsgCharacterFlag MsgClearFlag)
        ,("clr_country_flag"    , withNonlocAtom2 MsgCountryFlag MsgClearFlag)
        ,("clr_global_flag"     , withNonlocAtom2 MsgGlobalFlag MsgClearFlag)
        ,("clr_state_flag"      , withNonlocAtom2 MsgStateFlag MsgClearFlag)
        ,("clr_unit_leader_flag" , withNonlocAtom2 MsgUnitLeaderFlag MsgClearFlag)
        ,("clear_exiled_ruler"  , withNonlocAtom MsgClearExiledRuler)
        ,("clear_saved_name"    , withNonlocAtom MsgClearSavedName)
        ,("exile_heir_as"       , withNonlocAtom MsgExileHeir)
        ,("exile_ruler_as"      , withNonlocAtom MsgExileRuler)
        ,("exiled_same_dynasty_as_current" , withNonlocAtom MsgExiledRulerSameDynastyAsCurrent)
        ,("has_focus_tree"      , withNonlocAtom MsgHasFocusTree)
        ,("has_ruler"           , withNonlocAtom MsgHasRuler)
        ,("has_saved_event_target", withNonlocAtom MsgHasSavedEventTarget)
        ,("save_event_target_as", withNonlocAtom MsgSaveEventTargetAs)
        ,("save_global_event_target_as", withNonlocAtom MsgSaveGlobalEventTargetAs)
        ,("set_cosmetic_tag"    , withNonlocAtom MsgSetCosmeticTag)
        ,("set_heir"            , withNonlocAtom MsgSetHeir)
        ,("set_ruler"           , withNonlocAtom MsgSetRuler)
        ]

-- | Handlers for simple statements with icon
handlersSimpleIcon :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleIcon = Tr.fromList
        [("accepted_culture"        , withLocAtomAndIcon "max promoted cultures" MsgAcceptedCulture)
        ,("add_accepted_culture"    , withLocAtomAndIcon "max promoted cultures" MsgAddAcceptedCulture)
        ,("add_building"            , withLocAtomIconBuilding MsgAddBuilding)
        ,("add_harmonized_religion" , withLocAtomIcon MsgAddHarmonizedReligion)
        ,("add_heir_personality"    , withLocAtomIcon (MsgAddHeirPersonality False))
        ,("add_queen_personality"   , withLocAtomIcon (MsgAddConsortPersonality False))
        ,("add_reform_center"       , withLocAtomIcon MsgAddCenterOfReformation)
        ,("add_ruler_personality"   , withLocAtomIcon (MsgAddRulerPersonality False))
        ,("advisor"                 , withLocAtomIcon MsgHasAdvisorType)
        ,("can_build"               , withLocAtomIconBuilding MsgCanBuild)
        ,("change_cult"             , withLocAtomIcon MsgChangeCult)
        ,("change_personal_deity"   , withLocAtomIcon MsgSetPersonalDiety)
        ,("change_technology_group" , withLocAtomIcon MsgChangeTechGroup)
        ,("change_trade_goods"      , withLocAtomIcon MsgChangeGoods)
        ,("change_unit_type"        , withLocAtomIcon MsgChangeUnitType)
        ,("consort_has_personality" , withLocAtomIcon (MsgConsortHasPersonality False))
        ,("create_advisor"          , withLocAtomIcon MsgCreateAdvisor)
        ,("current_age"             , withLocAtomIcon MsgCurrentAge)
        ,("current_icon"            , withLocAtomIcon MsgCurrentIcon)
        ,("enable_religion"         , withLocAtomIcon MsgEnableReligion)
        ,("full_idea_group"         , withLocAtomIcon MsgFullIdeaGroup)
        ,("has_adopted_cult"        , withLocAtomIcon MsgHasAdoptedCult)
        ,("has_autonomy_state"      , withLocAtomIcon MsgHasAutonomyState)
        ,("has_building"            , withLocAtomIconBuilding MsgHasBuilding)
        ,("has_climate"             , withLocAtomIcon MsgHasClimate)
        ,("has_estate"              , withLocAtomIconHOI4Scope MsgEstateExists MsgHasEstate)
        ,("has_government"          , withLocAtomIcon MsgHasGovernment)
        ,("has_harmonized_with"     , withLocAtomIcon MsgHasHarmonizedWith)
        ,("has_institution"         , withLocAtomIcon MsgHasInstitution)
        ,("has_personal_deity"      , withLocAtomIcon MsgHasPersonalDiety)
        ,("has_unlocked_cult"       , withLocAtomIcon MsgHasUnlockedCult)
        ,("heir_has_personality"    , withLocAtomIcon (MsgHeirHasPersonality False))
        ,("holy_order"              , withLocAtomIcon MsgHasHolyOrder)
        ,("hre_religion"            , withLocAtomIcon MsgHREReligion)
        ,("is_institution_enabled"  , withLocAtomIcon MsgInstitutionEnabled)
        ,("is_monarch_leader"       , withLocAtomAndIcon "ruler general" MsgRulerIsGeneral)
        ,("is_religion_enabled"     , withLocAtomIcon MsgReligionEnabled)
        ,("remove_estate"           , withLocAtomIcon MsgRemoveFromEstate )
        ,("remove_ruler_personality" , withLocAtomIcon (MsgRemoveRulerPersonality False))
        ,("ruler_has_personality"   , withLocAtomIcon (MsgRulerHasPersonality False))
        ,("secondary_religion"      , withLocAtomIcon MsgSecondaryReligion)
        ,("set_estate"              , withLocAtomIcon MsgAssignToEstate)
        ,("set_hre_heretic_religion", withLocAtomIcon MsgSetHREHereticReligion)
        ,("set_hre_religion"        , withLocAtomIcon MsgSetHREReligion)
        ,("start_estate_agenda"     , withLocAtomIcon MsgStartEstateAgenda)
        ,("technology_group"        , withLocAtomIcon MsgTechGroup)
        ,("unlock_cult"             , withLocAtomIcon MsgUnlockCult)
        ]

-- | Handlers for simple statements with a flag or pronoun
handlersSimpleFlag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleFlag = Tr.fromList
        [("add_claim_by"            , withFlag MsgAddClaimBy)
        ,("add_core_of"             , withFlag MsgAddCoreOf)
        ,("add_historical_friend"   , withFlag MsgAddHistoricalFriend)
        ,("add_historical_rival"    , withFlag MsgAddHistoricalRival)
        ,("add_to_faction"          , withFlag MsgAddToFaction)
        ,("add_truce_with"          , withFlag MsgAddTruceWith)
        ,("adopt_reform_progress"   , withFlag MsgAdoptReformProgress)
        ,("alliance_with"           , withFlag MsgAlliedWith)
        ,("break_union"             , withFlag MsgBreakUnion)
        ,("cede_province"           , withFlag MsgCedeProvinceTo)
        ,("change_controller"       , withFlag MsgChangeController)
        ,("change_tag_from"         , withFlag MsgChangeTagFrom)
        ,("clear_estate_agenda_cache" , withFlag MsgClearEstateAgendaCache)
        ,("is_controlled_by"        , withFlag MsgIsControlledBy)
        ,("country_exists"          , withFlag MsgCountryExists)
        ,("country_or_non_sovereign_subject_holds" , withFlag MsgCountryOrNonSovereignSubjectHolds)
        ,("country_or_subject_holds" , withFlag MsgCountryOrSubjectHolds)
        ,("country_or_vassal_holds" , withFlag MsgCountryOrSubjectHolds) -- Legacy version of country_or_subject_holds
        ,("create_alliance"         , withFlag MsgCreateAlliance)
        ,("create_guarantee"        , withFlag MsgCreateGuarantee)
        ,("create_march"            , withFlag MsgCreateMarch)
        ,("create_marriage"         , withFlag MsgCreateMarriage)
        ,("create_union"            , withFlag MsgCreateUnion)
        ,("create_vassal"           , withFlag MsgCreateVassal)
        ,("has_defensive_war_with"      , withFlag MsgHasDefensiveWarWith)
        ,("discover_country"        , withFlag MsgDiscoverCountry)
        ,("excommunicate"           , withFlag MsgExcommunicate)
        ,("form_coalition_against"  , withFlag MsgFormCoalitionAgainst)
        ,("free_vassal"             , withFlag MsgFreeVassal)
        ,("galley"                  , withFlag MsgGalley)
        ,("give_guarantee"          , withFlag MsgGiveGuarantee)
        ,("give_military_access"    , withFlag MsgGiveMilitaryAccess)
        ,("guaranteed_by"           , withFlag MsgGuaranteedBy)
        ,("has_guaranteed"          , withFlag MsgHasGuaranteed)
        ,("has_merchant"            , withFlag MsgHasMerchant)
        ,("has_most_province_trade_power" , withFlag MsgHasMostProvinceTradePower)
        ,("has_non_aggression_pact_with" , withFlag MsgHasNonAggressionPactWith)
        ,("has_offensive_war_with"  , withFlag MsgHasOffensiveWarWith)
        ,("has_religious_school_of" , withFlag MsgHasReligiousSchoolOf)
        ,("has_subject"             , withFlag MsgHasSubject)
        ,("has_trader"              , withFlag MsgHasTrader)
        ,("heavy_ship"              , withFlag MsgHeavyShip)
        ,("historical_friend_with"  , withFlag MsgHistoricalFriendWith)
        ,("historical_rival_with"   , withFlag MsgHistoricalRivalWith)
        ,("has_pillaged_capital_against" , withFlag MsgHasPillagedCapitalAgainst)
        ,("humiliated_by"           , withFlag MsgHumiliatedBy)
        ,("inherit_technology"      , withFlag MsgInheritTechnology)
        ,("is_capital_of"           , withFlag MsgIsCapitalOf)
        ,("is_enemy"                , scope HOI4Country . withFlag MsgIsEnemy)
        ,("is_fully_controlled_by"  , withFlag MsgIsFullyControlledBy)
        ,("is_guaranteed_by"        , withFlag MsgIsGuaranteedBy)
--        ,("is_in_trade_league_with" , withFlag MsgIsInTradeLeagueWith)
        ,("is_in_faction_with"      , withFlag MsgIsInFactionWith)
        ,("is_justifying_wargoal_against" , withFlag MsgIsJustifyingWargoalAgainst)
        ,("is_league_enemy"         , withFlag MsgIsLeagueEnemy)
        ,("is_league_friend"        , withFlag MsgIsLeagueFriend)
        ,("is_neighbor_of"          , withFlag MsgNeighbors)
        ,("is_origin_of_consort"    , withFlag MsgIsOriginOfConsort)
        ,("is_owned_and_controlled_by" , withFlag MsgIsOwnedAndControlledBy)
        ,("is_puppet_of"            , withFlag MsgIsPuppetOf)
        ,("is_rival"                , withFlag MsgIsRival)
        ,("is_supporting_independence_of" , withFlag MsgIsSupportingIndependenceOf)
        ,("is_core_of"              , withFlag MsgIsStateCore)
        ,("is_strongest_trade_power", withFlag MsgIsStrongestTradePower)
        ,("is_subject_of"           , withFlag MsgIsSubjectOf)
        ,("is_threat"               , withFlag MsgIsThreat)
        ,("junior_union_with"       , withFlag MsgJuniorUnionWith)
        ,("knows_country"           , withFlag MsgKnowsCountry)
        ,("light_ship"              , withFlag MsgLightShip)
        ,("marriage_with"           , withFlag MsgRoyalMarriageWith)
        ,("overlord_of"             , withFlag MsgOverlordOf)
        ,("is_owned_by"             , withFlag MsgIsOwnedBy)
        ,("preferred_emperor"       , withFlag MsgPreferredEmperor)
        ,("provinces_on_capital_continent_of" , withFlag MsgProvincesOnCapitalContinentOf)
        ,("release"                 , withFlag MsgReleaseVassal)
        ,("remove_claim"            , withFlag MsgRemoveClaim)
        ,("remove_core_of"          , withFlag MsgRemoveCoreOf)
        ,("remove_from_faction"     , withFlag MsgRemoveFromFaction)
        ,("remove_historical_friend" , withFlag MsgRemoveHistoricalFriend)
        ,("remove_historical_rival" , withFlag MsgRemoveHistoricalRival)
        ,("senior_union_with"       , withFlag MsgSeniorUnionWith)
        ,("sieged_by"               , withFlag MsgUnderSiegeBy)
        ,("succession_claim"        , withFlag MsgSuccessionClaim)
        ,("support_independence_of" , withFlag MsgSupportIndependenceOf)
        ,("switch_tag"              , withFlag MsgSwitchTag)
        ,("tag"                     , withFlag MsgCountryIs)
        ,("trade_embargoing"        , withFlag MsgTradeEmbargoing)
        ,("trade_embargo_by"        , withFlag MsgEmbargoedBy)
        ,("truce_with"              , withFlag MsgTruceWith)
        ,("vassal_of"               , withFlag MsgVassalOf)
        ,("puppet"                  , withFlag MsgPuppet)
        ,("has_war_with"            , withFlag MsgHasWarWith)
        ,("has_war_together_with"   , withFlag MsgHasWarTogetherWith)
        ,("original_tag"            , withFlag MsgOrignalTag)
        ,("white_peace"             , withFlag MsgMakeWhitePeace)
        ]

-- | Handlers for simple generic statements with a flag or "yes"/"no"
handlersFlagOrYesNo :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrYesNo = Tr.fromList
        [("exists", withFlagOrBool MsgExists MsgCountryExists)
        ]

-- | Handlers for statements whose RHS may be an icon, a flag, a province, or a
-- pronoun (such as ROOT).
handlersIconFlagOrPronoun :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIconFlagOrPronoun = Tr.fromList
        [("change_culture"   , locAtomTagOrState (const MsgChangeCulture) MsgChangeSameCulture)
        -- above is province, below is country - use same messages for both
        ,("change_primary_culture", locAtomTagOrState (const MsgChangeCulture) MsgChangeSameCulture)
        ,("change_culture"   , locAtomTagOrState (const MsgChangeCulture) MsgChangeSameCulture)
        ,("change_religion"  , iconOrFlag MsgChangeReligion MsgChangeSameReligion Nothing)
        ,("consort_culture"  , locAtomTagOrState (const MsgConsortCultureIs) MsgConsortCultureIsSame)
        ,("continent"        , locAtomTagOrState (const MsgContinentIs) MsgContinentIsAs)
        ,("culture"          , locAtomTagOrState (const MsgCultureIs) MsgCultureIsAs)
        ,("culture_group"    , locAtomTagOrState (const MsgCultureIsGroup) MsgCultureGroupAs)
        ,("dominant_culture" , locAtomTagOrState (const MsgDominantCultureIs) MsgDominantCultureIsAs)
        ,("dominant_religion", locAtomTagOrState MsgDominantReligion MsgDominantReligionAs)
        ,("heir_culture"     , locAtomTagOrState (const MsgHeirCultureIs) MsgHeirCultureIsSame)
        ,("heir_nationality" , locAtomTagOrState (const MsgHeirNationality) MsgHeirNationalityAs)
        ,("heir_religion"    , locAtomTagOrState MsgHeirReligion MsgHeirReligionAs)
        ,("is_core"          , tagOrState MsgIsCoreOf MsgHasCoreOn (Just HOI4Country))
        ,("is_claim"         , tagOrState MsgHasClaim MsgHasClaimOn (Just HOI4Country))
        ,("is_permanent_claim" , tagOrState MsgIsPermanentClaim MsgHasPermanentClaim (Just HOI4Country))
        ,("primary_culture"  , locAtomTagOrState (const MsgPrimaryCultureIs) MsgPrimaryCultureIsAs)
        ,("province_religion" , locAtomTagOrState MsgProvinceReligion MsgProvinceSameReligion)
        ,("religion"         , locAtomTagOrState MsgReligion MsgSameReligion)
        ,("religion_group"   , locAtomTagOrState MsgReligionGroup MsgSameReligionGroup)
        ,("ruler_culture"    , iconOrFlag MsgRulerCultureIs MsgRulerCultureIsSame Nothing)
        ,("ruler_religion"   , iconOrFlag MsgRulerReligionIs MsgRulerReligionIsSame Nothing)
        ,("set_consort_culture" , locAtomTagOrState (const MsgChangeConsortCulture) MsgChangeConsortSameCulture)
        ,("set_heir_culture" , locAtomTagOrState (const MsgChangeHeirCulture) MsgChangeHeirSameCulture)
        ,("set_heir_religion", locAtomTagOrState MsgSetHeirReligion MsgSetHeirReligionAs)
        ,("set_ruler_culture" , locAtomTagOrState (const MsgChangeRulerCulture) MsgChangeRulerSameCulture)
        ,("set_ruler_religion" , iconOrFlag MsgChangeRulerReligion MsgChangeRulerSameReligion Nothing)
        ,("trade_goods"      , locAtomTagOrState MsgProducesGoods MsgProducesSameGoods)
        ]



-- | Handlers for yes/no statements
handlersYesNo :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersYesNo = Tr.fromList
        [("is_ai"                       , withBool MsgIsAIControlled)
        ,("allows_female_emperor"       , withBool MsgFemaleEmperorAllowed)
        ,("always"                      , withBool MsgAlways)
        ,("at_war_with_religious_enemy" , withBool MsgAtWarWithReligiousEnemy)
        ,("back_current_issue"          , withBool MsgBackCurrentIssue)
        ,("can_establish_order"         , withBool MsgCanEstablishHolyOrder)
        ,("elector"                     , withBool MsgElector)
        ,("expelling_minorities"        , withBool MsgExpellingMinorities)
        ,("has_active_debate"           , withBool MsgHasActiveDebate)
        ,("has_any_active_estate_agenda", withBool MsgHasAnyActiveEstateAgenda)
--        ,("has_any_disaster"            , withBool MsgHasAnyDisaster)
        ,("has_any_estates"             , withBool MsgHasAnyEstates)
        ,("has_active_fervor"           , withBool MsgHasActiveFervor)
        ,("has_cardinal"                , withBool MsgHasCardinal)
        ,("has_capitulated"             , withBool MsgHasCapitulated)
        ,("has_completed_all_reforms_trigger" , withBool MsgHasCompletedAllReforms)
        ,("has_final_tier_reforms_trigger" , withBool MsgHasFinalTierReform)
        ,("has_consort"                 , withBool MsgHasConsort)
        ,("has_consort_regency"         , withBool MsgHasConsortRegency)
        ,("has_civil_war"               , withBool MsgHasCivilWar)
        ,("has_custom_ideas"            , withBool MsgHasCustomIdeas)
        ,("has_defensive_war"           , withBool MsgHasDefensiveWar)
        ,("has_divert_trade"            , withBool MsgHasDivertTrade)
        ,("has_embargo_rivals"          , withBool MsgHasEmbargoRivals)
        ,("has_empty_adjacent_province" , withBool MsgHasEmptyAdjProvince)
        ,("has_factions"                , withBool MsgHasFactions)
        ,("has_female_consort"          , withBool MsgHasFemaleConsort)
        ,("has_female_heir"             , withBool MsgHasFemaleHeir)
        ,("has_flagship"                , withBool MsgHasFlagship)
        ,("has_foreign_consort"         , withBool MsgHasForeignConsort)
        ,("has_foreign_heir"            , withBool MsgHasForeignHeir)
        ,("has_had_golden_age"          , withBool MsgHasHadGoldenAge)
        ,("has_influencing_fort"        , withBool MsgHasInfluencingFort)
        ,("has_manufactory_trigger"     , withBool MsgHasAnyManufactory)
        ,("has_missionary"              , withBool MsgHasMissionary)
        ,("has_new_dynasty"             , withBool MsgHasNewDynasty)
        ,("has_offensive_war"           , withBool MsgHasOffensiveWar)
        ,("has_or_building_flagship"    , withBool MsgHasOrBuildingFlagship)
        ,("has_owner_accepted_culture"  , withBool MsgHasOwnerAcceptedCulture)
        ,("has_owner_culture"           , withBool MsgHasOwnerCulture)
        ,("has_owner_religion"          , withBool MsgHasOwnerReligion)
        ,("has_parliament"              , withBool MsgHasParliament)
        ,("has_port"                    , withBool MsgHasPort)
        ,("has_privateers"              , withBool MsgHasPrivateers)
        ,("has_regency"                 , withBool MsgIsInRegency)
        ,("has_religious_school"        , withBool MsgHasReligiousSchool)
        ,("has_revolution_in_province"  , withBool MsgHasRevolutionInProvince)
        ,("has_scutage"                 , withBool MsgHasScutage)
        ,("has_seat_in_parliament"      , withBool MsgHasSeatInParliament)
        ,("has_secondary_religion"      , withBool MsgHasSecondaryReligion)
        ,("has_send_officers"           , withBool MsgHasSendOfficers)
        ,("has_siege"                   , withBool MsgUnderSiege)
        ,("has_state_patriach"          , withBool MsgHasStatePatriach)
        ,("has_states_general_mechanic" , withBool MsgHasStatesGeneralMechanic)
        ,("has_subsidize_armies"        , withBool MsgHasSubsidizeArmies)
        ,("has_support_loyalists"       , withBool MsgHasSupportLoyalists)
        ,("has_truce"                   , withBool MsgHasTruce)
        ,("has_wartaxes"                , withBool MsgHasWarTaxes)
        ,("hre_leagues_enabled"         , withBool MsgHRELeaguesEnabled)
        ,("hre_religion_locked"         , withBool MsgHREReligionLocked)
        ,("hre_religion_treaty"         , withBool MsgHREWestphalia)
        ,("in_golden_age"               , withBool MsgInGoldenAge)
        ,("is_all_concessions_in_council_taken" , withBool MsgAllConcesssionsTaken)
        ,("has_war"                     , withBool MsgHasWar)
        ,("is_backing_current_issue"    , withBool MsgIsBackingCurrentIssue)
        ,("is_bankrupt"                 , withBool MsgIsBankrupt)
        ,("is_blockaded"                , withBool MsgIsBlockaded)
        ,("is_capital"                  , withBool MsgIsCapital)
        ,("is_center_of_revolution"     , withBool MsgIsCenterOfRevolution)
        ,("is_city"                     , withBool MsgIsCity)
        ,("is_coastal"                  , withBool MsgIsCoastal)
        ,("is_colonial_nation"          , withBool MsgIsColonialNation)
        ,("is_colony"                   , withBool MsgIsColony)
        ,("is_council_enabled"          , withBool MsgIsCouncilEnabled)
        ,("is_defender_of_faith"        , withBool MsgIsDefenderOfFaith)
        ,("is_demilitarized_zone"       , withBool MsgIsDemilitarizedZone )
        ,("is_elector"                  , withBool MsgIsElector)
        ,("is_emperor"                  , withBool MsgIsEmperor)
        ,("is_emperor_of_china"         , withBool MsgIsEmperorOfChina)
        ,("is_empty"                    , withBool MsgIsProvinceEmpty)
        ,("is_excommunicated"           , withBool MsgIsExcommunicated)
        ,("is_faction_leader"           , withBool MsgIsFactionLeader)
        ,("is_federation_leader"        , withBool MsgIsFederationLeader)
        ,("is_federation_nation"        , withBool MsgIsFederationNation)
        ,("is_female"                   , withBool MsgIsFemale)
        ,("is_foreign_company"          , withBool MsgIsForeignCompany)
        ,("is_force_converted"          , withBool MsgWasForceConverted)
        ,("is_former_colonial_nation"   , withBool MsgIsFormerColonialNation)
        ,("is_free_or_tributary_trigger", withBool MsgIsFreeOrTributaryTrigger)
        ,("is_great_power"              , withBool MsgIsGreatPower)
        ,("is_heir_leader"              , withBool MsgIsHeirLeader)
        ,("is_historical_focus_on"      , withBool MsgIsHistoricalFocusOn)
        ,("is_in_capital_area"          , withBool MsgIsInCapitalArea)
        ,("is_in_coalition"             , withBool MsgIsInCoalition)
        ,("is_in_deficit"               , withBool MsgIsInDeficit)
        ,("is_in_extended_regency"      , withBool MsgIsInExtendedRegency)
        ,("is_in_faction"               , withBool MsgIsInFaction)
        ,("is_in_home_area"             , withBool MsgIsInHomeArea)
        ,("is_in_league_war"            , withBool MsgIsInLeagueWar)
        ,("is_iroquois"                 , withBool MsgIsIroquois)
        ,("is_island"                   , withBool MsgIsIsland)
        ,("is_league_leader"            , withBool MsgIsLeagueLeader)
        ,("is_lesser_in_union"          , withBool MsgIsLesserInUnion)
        ,("is_looted"                   , withBool MsgIsLooted)
        ,("is_march"                    , withBool MsgIsMarch)
        ,("is_major"                    , withBool MsgIsMajor)
        ,("is_node_in_trade_company_region" , withBool MsgTradeNodeIsInTCRegion)
        ,("is_nomad"                    , withBool MsgIsNomad)
        ,("is_orangists_in_power"       , withBool MsgIsOrangistsInPower)
        ,("is_overseas"                 , withBool MsgIsOverseas)
        ,("is_owned_by_trade_company"   , withBool MsgIsOwnedByTradeCompany)
        ,("is_papal_controller"         , withBool MsgIsPapalController)
        ,("is_part_of_hre"              , withBool MsgIsPartOfHRE)
        ,("is_playing_custom_nation"    , withBool MsgIsCustomNation)
        ,("is_prosperous"               , withBool MsgIsProsperous)
        ,("is_puppet"                   , withBool MsgIsPuppet)
        ,("is_random_new_world"         , withBool MsgRandomNewWorld)
        ,("is_reformation_center"       , withBool MsgIsCenterOfReformation)
        ,("is_religion_reformed"        , withBool MsgReligionReformed)
        ,("is_religious_center_province" , withBool MsgIsReligiousCenterProvince)
        ,("is_revolution_target"        , withBool MsgIsRevolutionTarget)
        ,("is_revolutionary"            , withBool MsgIsRevolutionary)
        ,("is_revolutionary_republic_trigger" , withBool MsgIsRevolutionaryRepublic)
        ,("is_sea"                      , withBool MsgIsSea) -- province or trade node
        ,("is_statists_in_power"        , withBool MsgIsStatistsInPower)
        ,("is_subject"                  , withBool MsgIsSubject)
        ,("is_territory"                , withBool MsgIsTerritory)
        ,("is_trade_league_leader"      , withBool MsgIsTradeLeagueLeader)
        ,("is_tribal"                   , withBool MsgIsTribal)
        ,("is_tutorial_active"          , withBool MsgIsInTutorial)
        ,("is_unit_leader"              , withBool MsgIsUnitLeader)
        ,("is_vassal"                   , withBool MsgIsVassal)
        ,("is_wasteland"                , withBool MsgIsWasteland)
        ,("luck"                        , withBool MsgLucky)
        ,("normal_or_historical_nations", withBool MsgNormalOrHistoricalNations)
        ,("papacy_active"               , withBool MsgPapacyIsActive)
        ,("primitives"                  , withBool MsgPrimitives)
        ,("province_has_current_tech_fort_trigger" , withBool MsgProvinceHasCurrentTechFort)
        ,("revolution_target_exists"    , withBool MsgRevolutionTargetExists)
        ,("recent_treasure_ship_passage", withBool MsgRecentTreasureShipPassage)
        ,("ruler_is_foreigner"          , withBool MsgRulerIsForeigner)
        ,("set_demilitarized_zone"      , withBool MsgSetDemilitarizedZone)
        ,("set_hre_religion_locked"     , withBool MsgSetHREReligionLocked)
        ,("set_in_empire"               , withBool MsgSetInEmpire)
        ,("set_seat_in_parliament"      , withBool MsgSetSeatInParliament)
        ,("set_revolution_in_province"  , withBool MsgSetRevolutionProvince)
        ,("uses_religious_icons"        , withBool MsgUsesIcons)
        ,("unit_in_siege"               , withBool MsgUnderSiege) -- duplicate?
        ,("uses_devotion"               , withBool MsgUsesDevotion)
        ,("uses_doom"                   , withBool MsgUsesDoom)
        ,("uses_piety"                  , withBool MsgUsesPiety)
        ,("valid_for_personal_unions_trigger" , withBool MsgValidForPU)
        ,("was_player"                  , withBool MsgHasBeenPlayer)
        ,("was_never_end_game_tag_trigger" , withBool MsgWasNeverEndGameTag)

        -- Scripted triggers for advisors
        ,("has_adm_advisor"             , boolIconLoc "adm" "ADM" MsgHasAdvisorCategory)
        ,("has_dip_advisor"             , boolIconLoc "dip" "DIP" MsgHasAdvisorCategory)
        ,("has_mil_advisor"             , boolIconLoc "mil" "MIL" MsgHasAdvisorCategory)
        ,("has_adm_advisor_2"           , boolIconLoc "adm" "ADM" (MsgHasAdvisorCategoryLevel 2))
        ,("has_dip_advisor_2"           , boolIconLoc "dip" "DIP" (MsgHasAdvisorCategoryLevel 2))
        ,("has_mil_advisor_2"           , boolIconLoc "mil" "MIL" (MsgHasAdvisorCategoryLevel 2))
        ,("has_adm_advisor_3"           , boolIconLoc "adm" "ADM" (MsgHasAdvisorCategoryLevel 3))
        ,("has_dip_advisor_3"           , boolIconLoc "dip" "DIP" (MsgHasAdvisorCategoryLevel 3))
        ,("has_mil_advisor_3"           , boolIconLoc "mil" "MIL" (MsgHasAdvisorCategoryLevel 3))

        -- From common/scripted_effects/01_scripted_triggers_muslim_schools.txt
        -- A bit lazy, but they're not commonly used
        --
        ,("has_sufi_sheikh_ul_islam_trigger"       , withBool (MsgRulerHasIslamModifier "Sufi"))
        ,("has_pious_sheikh_ul_islam_trigger"      , withBool (MsgRulerHasIslamModifier "Righteous"))
        ,("has_loyal_sheikh_ul_islam_trigger"      , withBool (MsgRulerHasIslamModifier "Loyal"))
        ]

-- | Handlers for statements that may be numeric or a tag
handlersNumericOrTag :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericOrTag = Tr.fromList
        [("num_of_cities"       , numericOrTag MsgNumCities MsgNumCitiesThan)
        ,("army_professionalism", numericOrTagIcon "army professionalism" MsgArmyProfessionalism MsgArmyProfessionalismAs)
        ,("inflation"           , numericOrTagIcon "inflation" MsgInflation MsgInflationAs)
        ,("total_development"   , numericOrTagIcon "development" MsgTotalDevelopment MsgTotalDevelopmentAs)
        ,("total_own_and_non_tributary_subject_development" , numericOrTagIcon "development" MsgOwnOrNonTribSubjectDevelopment MsgOwnOrNonTribSubjectDevelopmentAs)
        ,("num_of_artillery"    , numericOrTag MsgNumArtillery MsgNumArtilleryThan)
        ,("num_of_cavalry"      , numericOrTag MsgNumCavalry MsgNumCavalryThan)
        ]

-- | Handlers for signed numeric statements
handlersSignedNumeric :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSignedNumeric = Tr.fromList
        [("tolerance_to_this", numeric MsgToleranceToThis)
        ]

-- | Handlers querying the number of provinces of some kind, mostly religions
-- and trade goods
handlersNumStates :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumStates = Tr.fromList
        [
        -- Religions
         ("animism"       , numProvinces "animism" MsgReligionProvinces)
        ,("catholic"      , numProvinces "catholic" MsgReligionProvinces)
        ,("confucianism"  , numProvinces "confucianism" MsgReligionProvinces)
        ,("coptic"        , numProvinces "coptic" MsgReligionProvinces)
        ,("hinduism"      , numProvinces "hinduism" MsgReligionProvinces)
        ,("ibadi"         , numProvinces "ibadi" MsgReligionProvinces)
        ,("mahayana"      , numProvinces "mahayana" MsgReligionProvinces)
        ,("orthodox"      , numProvinces "orthodox" MsgReligionProvinces)
        ,("protestant"    , numProvinces "protestant" MsgReligionProvinces)
        ,("reformed"      , numProvinces "reformed" MsgReligionProvinces)
        ,("shamanism"     , numProvinces "fetishism" MsgReligionProvinces) -- Old name for fetishism
        ,("shiite"        , numProvinces "shiite" MsgReligionProvinces)
        ,("shinto"        , numProvinces "shinto" MsgReligionProvinces)
        ,("sikhism"       , numProvinces "sikhism" MsgReligionProvinces)
        ,("sunni"         , numProvinces "sunni" MsgReligionProvinces)
        ,("totemism"      , numProvinces "totemism" MsgReligionProvinces)

        -- Trade goods
        ,("coal"          , numProvinces "coal" MsgGoodsProvinces)
        ,("chinaware"     , numProvinces "chinaware" MsgGoodsProvinces)
        ,("cloth"         , numProvinces "cloth" MsgGoodsProvinces)
        ,("cocoa"         , numProvinces "cocoa" MsgGoodsProvinces)
        ,("coffee"        , numProvinces "coffee" MsgGoodsProvinces)
        ,("copper"        , numProvinces "copper" MsgGoodsProvinces)
        ,("cotton"        , numProvinces "cotton" MsgGoodsProvinces)
        ,("fish"          , numProvinces "fish" MsgGoodsProvinces)
        ,("fur"           , numProvinces "fur" MsgGoodsProvinces)
        ,("glass"         , numProvinces "glass" MsgGoodsProvinces)
        ,("gold"          , numProvinces "gold" MsgGoodsProvinces)
        ,("grain"         , numProvinces "grain" MsgGoodsProvinces)
        ,("iron"          , numProvinces "iron" MsgGoodsProvinces)
        ,("ivory"         , numProvinces "ivory" MsgGoodsProvinces)
        ,("livestock"     , numProvinces "livestock" MsgGoodsProvinces)
        ,("naval_supplies", numProvinces "naval supplies" MsgGoodsProvinces)
        ,("salt"          , numProvinces "salt" MsgGoodsProvinces)
        ,("silk"          , numProvinces "silk" MsgGoodsProvinces)
        ,("slaves"        , numProvinces "slaves" MsgGoodsProvinces)
        ,("spices"        , numProvinces "spices" MsgGoodsProvinces)
        ,("sugar"         , numProvinces "sugar" MsgGoodsProvinces)
        ,("tea"           , numProvinces "tea" MsgGoodsProvinces)
        ,("tobacco"       , numProvinces "tobacco" MsgGoodsProvinces)
        ,("wine"          , numProvinces "wine" MsgGoodsProvinces)
        ,("wool"          , numProvinces "wool" MsgGoodsProvinces)
        ]

-- Helpers for text/value pairs
tryLocAndIconTitle t = tryLocAndIcon (t <> "_title")

-- | Handlers for text/value pairs.
--
-- $textvalue
handlersTextValue :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextValue = Tr.fromList
        [("add_great_project_tier"      , textValue "type" "tier" MsgAddGreatProjectTier MsgAddGreatProjectTier tryLocAndIcon)
        ,("add_incident_variable_value" , textValue "incident" "value" MsgAddIncidentVariableValue MsgAddIncidentVariableValue tryLocAndIconTitle)
        ,("add_institution_embracement" , textValue "which" "value" MsgAddInstitutionEmbracement MsgAddInstitutionEmbracement tryLocAndIcon)
--        ,("add_disaster_progress"       , textValue "disaster" "value" MsgAddDisasterProgress MsgAddDisasterProgress tryLocAndIcon)
        ,("add_estate_loyalty"          , textValue "estate" "loyalty" MsgAddEstateLoyalty MsgAddEstateLoyalty tryLocAndIcon)
        ,("add_named_unrest"            , textValue "name" "value" MsgAddNamedUnrest MsgAddNamedUnrest tryLocAndIcon)
        ,("add_offsite_building"        , textValue "type" "level" MsgAddOffsiteBuilding  MsgAddOffsiteBuilding tryLocAndIcon)
        ,("add_popularity"              , textValue "ideology" "popularity" MsgAddPopularity MsgAddPopularity tryLocAndIcon)
        ,("add_power_projection"        , textValue "type" "amount" MsgAddPowerProjection MsgAddPowerProjection tryLocAndIcon)
        ,("add_spy_network_from"        , textValue "who" "value" MsgAddSpyNetworkFrom MsgAddSpyNetworkFrom flagTextMaybe)
        ,("add_spy_network_in"          , textValue "who" "value" MsgAddSpyNetworkIn MsgAddSpyNetworkIn flagTextMaybe)
        ,("army_strength"               , textValue "who" "value" MsgArmyStrength MsgArmyStrength flagTextMaybe)
        ,("border_distance"             , textValue "who" "distance" MsgBorderDistance MsgBorderDistance flagTextMaybe)
        ,("estate_influence"            , textValue "estate" "influence" MsgEstateInfluence MsgEstateInfluence tryLocAndIcon)
        ,("estate_loyalty"              , textValue "estate" "loyalty" MsgEstateLoyalty MsgEstateLoyalty tryLocAndIcon)
        ,("estate_territory"            , textValue "estate" "territory" MsgEstateTerritory MsgEstateTerritory tryLocAndIcon)
        ,("has_global_modifier_value"   , textValue "which" "value" MsgHasGlobalModifierValue MsgHasGlobalModifierValue tryLocAndLocMod)
        ,("has_spy_network_from"        , textValue "who" "value" MsgHasSpyNetworkFrom MsgHasSpyNetworkFrom flagTextMaybe)
        ,("has_spy_network_in"          , textValue "who" "value" MsgHasSpyNetworkIn MsgHasSpyNetworkIn flagTextMaybe)
        ,("has_volunteers_amount_from"  , textValueCompare "tag" "count" "more than" "less than" MsgHasVolunteersAmountFrom MsgHasVolunteersAmountFrom flagTextMaybe)
        ,("has_won_war_against"         , textValue "who" "max_years_since" MsgHasWonWarAgainst MsgHasWonWarAgainst flagTextMaybe)
        ,("incident_variable_value"     , textValue "incident" "value" MsgIncidentVariableValue MsgIncidentVariableValue tryLocAndIconTitle)
        ,("institution_difference"      , textValue "who" "value" MsgInstitutionDifference MsgInstitutionDifference flagTextMaybe)
        ,("military_strength"           , textValue "who" "value" MsgMilitaryStrength MsgMilitaryStrength flagTextMaybe)
        ,("modify_tech_sharing_bonus"   , textValue "id" "bonus" MsgModifyTechSharingBonus MsgModifyTechSharingBonus tryLocMaybe)
        ,("num_of_estate_privileges"    , textValue "estate" "value" MsgNumEstatePrivileges MsgNumEstatePrivileges tryLocAndIcon)
        ,("num_of_units_in_province"    , textValue "who" "amount" MsgNumUnitsInProvince MsgNumUnitsInProvince flagTextMaybe) -- TODO: Support type
        ,("num_investments_in_trade_company_region" , textValue "investment" "value" MsgNumInvestmentsInTradeCompanyReigion MsgNumInvestmentsInTradeCompanyReigion tryLocAndIcon)
        ,("naval_strength"              , textValue "who" "value" MsgNavalStrength MsgNavalStrength flagTextMaybe)
        ,("province_distance"           , textValue "who" "distance" MsgProvinceDistance MsgProvinceDistance flagTextMaybe)
        ,("school_opinion"              , textValue "who" "opinion" MsgSchoolOpinion MsgSchoolOpinion flagTextMaybe)
        ,("set_province_name"           , textValue "name" "id" MsgSetProvinceName MsgSetProvinceName tryLocMaybe)
        ,("set_school_opinion"          , textValue "who" "opinion" MsgSetSchoolOpinion MsgSetSchoolOpinion flagTextMaybe)
        ,("set_victory_points"          , valueValue "province" "value" MsgSetVictoryPoints MsgSetVictoryPoints)
        ,("strength_ratio"              , textValueCompare "tag" "ratio" "more than" "less than" MsgStrengthRatio MsgStrengthRatio flagTextMaybe)
        ,("remove_building"             , textValue "type" "level" MsgRemoveBuilding MsgRemoveBuilding tryLocAndIcon)
        ,("remove_loot"                 , textValue "who" "amount" MsgRemoveLoot MsgRemoveLoot flagTextMaybe)
        ,("trade_goods_produced_amount" , textValue "trade_goods" "amount" MsgTradeGoodsProduced MsgTradeGoodsProduced tryLocAndIcon)
        ,("trading_part"                , textValue "trade_goods" "value" MsgTradingPart MsgTradingPart tryLocAndIcon)
        ,("trade_share"                 , textValue "country" "share" MsgTradeShare MsgTradeShare flagTextMaybe)
        ,("trust"                       , textValue "who" "value" MsgTrust MsgTrust flagTextMaybe)
        ,("war_score_against"           , textValue "who" "value" MsgWarscoreAgainst MsgWarscoreAgainst flagTextMaybe)
        ,("years_in_union_under"        , textValue "who" "years" MsgYearsInUnionUnder MsgYearsInUnionUnder flagTextMaybe)
        ,("modify_character_flag"       , withNonlocTextValue2 "flag" "value" MsgCharacterFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_country_flag"         , withNonlocTextValue2 "flag" "value" MsgCountryFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_global_flag"          , withNonlocTextValue2 "flag" "value" MsgGlobalFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_state_flag"           , withNonlocTextValue2 "flag" "value" MsgStateFlag MsgModifyFlag) -- Localization/icon ignored
        ,("modify_unit_leader_flag"     , withNonlocTextValue2 "flag" "value" MsgUnitLeaderFlag MsgModifyFlag) -- Localization/icon ignored
        ]

-- | Handlers for text/atom pairs
handlersTextAtom :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextAtom = Tr.fromList
        [("create_flagship"             , taDescAtomIcon "name" "type" MsgCreateNamedShip)
        ,("create_named_ship"           , taDescAtomIcon "name" "type" MsgCreateFlagShip)
        ,("has_game_rule"               , textAtom "rule" "option" MsgHasRule tryLoc)
        ,("pick_random_estate_if_present" , textAtom "flag" "estate_action" MsgPickRandomEstateIfPresent tryLoc) -- Localization/icon ignored
        ,("religious_school"            , textAtom "school" "group" MsgReligiousSchool tryLoc)
        ,("set_religious_school"        , textAtom "school" "group" MsgSetReligiousSchool tryLoc)
        ]

-- | Handlers for special complex statements
handlersSpecialComplex :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersSpecialComplex = Tr.fromList
        [("add_building_construction"    , addBuildingConstruction)
        ,("add_casus_belli"              , addCB True)
        ,("add_doctrine_cost_reduction"  , addDoctrineCostReduction)
        ,("add_faction_influence"        , factionInfluence MsgFactionGainInfluence)
        ,("add_government_power"         , governmentPower)
        ,("add_estate_influence_modifier", estateInfluenceModifier MsgEstateInfluenceModifier)
        ,("add_mutual_opinion_modifier_effect", opinion MsgMutualOpinion MsgMutualOpinionDur)
        ,("add_named_threat"             , addNamedThreat)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion MsgAddOpinionDur)
        ,("add_unit_construction"        , addUnitConstruction)
        ,("add_tech_bonus"               , addTechBonus)
        ,("add_to_war"                   , addToWar)
        ,("add_trust"                    , trust)
        ,("ai_attitude"                  , aiAttitude MsgAiAttitude)
        ,("annex_country"                , annexCountry)
        ,("reverse_add_opinion_modifier" , opinion MsgReverseAddOpinion MsgReverseAddOpinionDur)
        ,("area"                         , area)
        ,("build_railway"                , buildRailway)
        ,("can_build_railway"            , canBuildRailway)
        ,("change_price"                 , changePrice)
        ,("create_admiral"               , createMilitaryLeader "admiral" True MsgCreateAdmiral MsgDefineAdmiral)
        ,("create_conquistador"          , createMilitaryLeader "conquistador" False MsgCreateConquistador MsgDefineConquistador)
        ,("create_explorer"              , createMilitaryLeader "explorer" True MsgCreateExplorer MsgDefineExplorer)
        ,("create_general"               , createMilitaryLeader "general" False MsgCreateGeneral MsgDefineGeneral)
        ,("create_equipment_variant"     , createEquipmentVariant)
        ,("create_wargoal"               , createWargoal)
        ,("custom_trigger_tooltip"       , customTriggerTooltip)
        ,("build_to_forcelimit"          , buildToForcelimit)
        ,("country_event"                , scope HOI4Country . triggerEvent MsgCountryEvent)
        ,("declare_war_with_cb"          , declareWarWithCB)
        ,("declare_war_on"               , declareWarOn)
        ,("define_advisor"               , defineAdvisor False)
        ,("define_consort"               , defineConsort)
        ,("define_exiled_ruler"          , defineExiledRuler)
        ,("define_heir"                  , defineHeir)
        ,("define_ruler"                 , defineRuler)
        ,("define_admiral"               , defineMilitaryLeader "admiral" True MsgDefineAdmiral)
        ,("define_conquistador"          , defineMilitaryLeader "conquistador" False MsgDefineConquistador)
        ,("define_explorer"              , defineMilitaryLeader "explorer" True MsgDefineExplorer)
        ,("define_general"               , defineMilitaryLeader "general" False MsgDefineGeneral)
        ,("define_leader_to_ruler"       , defineDynMember (\_ -> MsgDefinerLeaderToRuler) (\_ -> \_ -> MsgDefinerLeaderToRuler) (\_ -> MsgDefinerLeaderToRuler) (\_ -> \_ -> MsgDefinerLeaderToRuler))
        ,("define_ruler_to_general"      , defineMilitaryLeader "general" False MsgDefineRulerToGeneral)
        ,("employed_advisor"             , employedAdvisor)
        ,("expulsion_target"             , expulsionTarget)
        ,("faction_influence"            , factionInfluence MsgFactionHasInfluence)
        ,("free_building_slots"          , freeBuildingSlots)
        ,("has_completed_focus"          , handleFocus MsgHasCompletedFocus)
        ,("complete_national_focus"      , handleFocus MsgCompleteNationalFocus)
        ,("focus"                        , handleFocus MsgFocus) -- used in pre-requisite for focuses
        ,("focus_progress"               , focusProgress MsgFocusProgress)
        ,("has_army_size"                , hasArmySize)
        ,("has_estate_led_regency"       , hasEstateLedRegency)
        ,("has_estate_influence_modifier", hasEstateModifier MsgEstateHasInfluenceModifier)
        ,("has_estate_loyalty_modifier"  , hasEstateModifier MsgEstateHasLoyaltyModifier)
        ,("has_great_project"            , hasGreatProject)
        ,("has_idea"                     , handleIdeas False MsgHasIdea)
        ,("add_ideas"                    , handleIdeas True MsgAddIdea)
        ,("remove_ideas"                 , handleIdeas False MsgRemoveIdea)
        ,("add_timed_idea"               , handleTimedIdeas MsgAddTimedIdea)
        ,("modify_timed_idea"            , handleTimedIdeas MsgModifyTimedIdea)
        ,("swap_ideas"                   , handleSwapIdeas)
        ,("has_opinion"                  , hasOpinion MsgHasOpinion)
        ,("has_country_leader"           , hasCountryLeader)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion (\modid what who _years -> MsgAddOpinion modid what who))
        ,("has_reached_government_reform_tier" , hasGovernmentReforTier)
        ,("has_trade_company_investment_in_area", hasTradeCompanyInvestment)
        ,("is_in_war"                    , isInWar)
        ,("load_focus_tree"              , loadFocusTree)
        ,("modify_building_resources"    , modifyBuildingResources)
        ,("news_event"                   , scope HOI4Country . triggerEvent MsgNewsEvent)
        ,("privateer_power"              , privateerPower)
        ,("region"                       , region)
        ,("remove_opinion_modifier"      , opinion MsgRemoveOpinionMod (\modid what who _years -> MsgRemoveOpinionMod modid what who))
--        ,("reverse_has_opinion"          , hasOpinion MsgReverseHasOpinion)
        ,("reverse_has_opinion_modifier" , opinion MsgReverseHasOpinionMod (\modid what who _years -> MsgReverseHasOpinionMod modid what who))
        ,("reverse_remove_opinion"       , opinion MsgReverseRemoveOpinionMod (\modid what who _years -> MsgReverseRemoveOpinionMod modid what who))
        ,("religion_years"               , religionYears)
        ,("set_ai_attitude"              , aiAttitude MsgSetAiAttitude)
        ,("set_autonomy"                 , setAutonomy)
        ,("set_politics"                 , setPolitics)
        ,("set_party_name"               , setPartyName)
        ,("start_civil_war"              , startCivilWar)
        ,("state_event"                  , scope HOI4ScopeState . triggerEvent MsgStateEvent)
        ,("reverse_add_casus_belli"      , addCB False)
        ,("trading_bonus"                , tradingBonus)
        ,("trading_policy_in_node"       , tradingPolicyInNode)
        ,("trigger_switch"               , triggerSwitch)
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

        -- Effects
        ,("add_loot_from_rich_province_general_effect" , simpleEffectAtom "looter" MsgAddLootFromRichProvince) -- Note: RHS ignored
        ,("allow_baseline_invite_scholar"   , simpleEffectAtom "religious_school" MsgAllowBaselineInviteScholar)
        ,("check_reducing_estate_revolt_size_trigger" , simpleEffectAtom "flag" (MsgCheckEstateRevoltSize False))
        ,("check_reducing_estate_revolt_size_more_trigger" , simpleEffectAtom "flag" (MsgCheckEstateRevoltSize True))
        ,("generate_exile_advisor_effect"   , simpleEffectAtom "advisor_type" MsgGenerateExileAdvisor)
        ,("generate_scaled_advisor_of_type_and_religion_effect" , defineAdvisor True)
        ,("generate_traitor_advisor_effect" , simpleEffectNum "skill_level" MsgGenerateTraitorAdvisor)
        ,("has_primary_cult"                , simpleEffectAtom "cult" MsgHasPrimaryCult)
        ,("our_scholar_matches_their_school_trigger" , simpleEffectAtom "school" MsgOurScholarMatchesTheirSchool)
        ,("select_primary_cult"             , simpleEffectAtom "cult" MsgSelectPrimaryCult)
        ,("set_great_project_tier_1"        , simpleEffectAtom "type" (MsgSetGreatProjectTier 1))
        ,("set_great_project_tier_2"        , simpleEffectAtom "type" (MsgSetGreatProjectTier 2))
        ,("set_great_project_tier_3"        , simpleEffectAtom "type" (MsgSetGreatProjectTier 3))
        ,("set_nationality"                 , setNationality)
        ,("scaled_estate_land_share_reduce_effect" , simpleEffectAtom "estate" (MsgScaledEstateLandShareEffect False))
        ,("scaled_estate_land_share_add_effect" , simpleEffectAtom "estate" (MsgScaledEstateLandShareEffect True))
        ,("unlock_estate_privilege"         , simpleEffectAtom "estate_privilege" MsgUnlockEstatePrivilege)

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

        -- Has building triggers (from common/scripted_triggers/00_scripted_triggers.txt)
        ,("has_courthouse_building_trigger" , hasBuildingTrigger ["courthouse", "town_hall"])
        ,("has_dock_building_trigger"       , hasBuildingTrigger ["dock", "drydock"])
        ,("has_forcelimit_building_trigger" , hasBuildingTrigger ["regimental_camp", "conscription_center"])
        ,("has_manpower_building_trigger"   , hasBuildingTrigger ["barracks", "training_fields"])
        ,("has_production_building_trigger" , hasBuildingTrigger ["workshop", "counting_house"])
        ,("has_shipyard_building_trigger"   , hasBuildingTrigger ["shipyard", "grand_shipyard"])
        ,("has_tax_building_trigger"        , hasBuildingTrigger ["temple", "cathedral"])
        ,("has_trade_building_trigger"      , hasBuildingTrigger ["marketplace", "trade_depot", "stock_exchange"])
        ]

-- | Handlers for statements pertaining to rebels
handlersRebels :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersRebels = Tr.fromList
        [("can_spawn_rebels"  , canSpawnRebels)
        ,("create_revolt"     , spawnRebels Nothing)
        ,("has_spawned_rebels", hasSpawnedRebels)
        ,("likely_rebels"     , canSpawnRebels)
        ,("spawn_rebels"      , spawnRebels Nothing)
        -- Specific rebels
        ,("anti_tax_rebels"      , spawnRebelsSimple)
        ,("nationalist_rebels"   , spawnRebelsSimple)
        ,("noble_rebels"         , spawnRebelsSimple)
        ,("pretender_rebels"     , spawnRebelsSimple)
        ,("polish_noble_rebels"  , spawnRebelsSimple)
        ,("revolutionary_rebels" , spawnRebelsSimple)
        ,("heretic_rebels"       , spawnRebelsSimple)
        ,("particularist_rebels" , spawnRebelsSimple)
        -- Scripted
        ,("spawn_small_scaled_rebels" , spawnScaledRebels False)
        ,("spawn_large_scaled_rebels" , spawnScaledRebels True)
        ]

-- | Handlers for idea groups
handlersIdeaGroups :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersIdeaGroups = Tr.fromList
        -- Idea groups
        [--("aristocracy_ideas"   , hasIdea MsgHasAristocraticIdea)
--        ,("defensive_ideas"     , hasIdea MsgHasDefensiveIdea)
--        ,("economic_ideas"      , hasIdea MsgHasEconomicIdea)
--        ,("innovativeness_ideas", hasIdea MsgHasInnovativeIdea)
--        ,("maritime_ideas"      , hasIdea MsgHasMaritimeIdea)
--        ,("offensive_ideas"     , hasIdea MsgHasOffensiveIdea)
        ]

-- | Handlers for miscellaneous statements
handlersMisc :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersMisc = Tr.fromList
        [("add_autonomy_ratio"          , addAutonomyRatio)
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
        ,("faction_in_power"    , factionInPower)
        ,("government_rank"     , govtRank)
        ,("has_dlc"             , hasDlc)
        ,("has_equipment"       , hasEquipment)
        ,("has_government_attribute" , hasGovermentAttribute)
        ,("has_heir"            , hasHeir)
        ,("has_leader_with"     , hasLeaderWith)
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
        -- Ancestor personality
        ,("add_heir_personality_ancestor"    , personalityAncestor (MsgAddHeirPersonality True))
        ,("add_queen_personality_ancestor"   , personalityAncestor (MsgAddConsortPersonality True))
        ,("add_ruler_personality_ancestor"   , personalityAncestor (MsgAddRulerPersonality True))
        ,("consort_has_personality_ancestor" , personalityAncestor (MsgConsortHasPersonality True))
        ,("heir_has_personality_ancestor"    , personalityAncestor (MsgHeirHasPersonality True))
        ,("remove_ruler_personality_ancestor", personalityAncestor (MsgRemoveRulerPersonality True))
        ,("ruler_has_personality_ancestor"   , personalityAncestor (MsgRulerHasPersonality True))
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
ppOne' stmt lhs rhs = case lhs of
    GenericLhs label _ -> case Tr.lookup (TE.encodeUtf8 (T.toLower label)) ppHandlers of
        Just handler -> handler stmt
        -- default
        Nothing -> if isTag label
             then case rhs of
                CompoundRhs scr ->
                    withCurrentIndent $ \_ -> do -- force indent level at least 1
                        lflag <- plainMsg' =<< (<> ":") <$> flagText (Just HOI4Country) label
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
