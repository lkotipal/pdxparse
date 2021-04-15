{-|
Module      : EU4.Common
Description : Message handler for Europa Universalis IV
-}
module EU4.Common (
        pp_script
    ,   pp_mtth
    ,   ppOne
    ,   ppMany
    ,   iconKey, iconFile, iconFileB
    ,   AIWillDo (..), AIModifier (..)
    ,   ppAiWillDo, ppAiMod
    ,   extractStmt, matchLhsText
    ,   module EU4.Types
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
import Messages -- everything
import MessageTools (plural)
import QQ (pdx)
import SettingsTypes -- everything
import EU4.Handlers -- everything
import EU4.Types -- everything

-- no particular order from here... TODO: organize this!

-- | Format a script as wiki text.
pp_script :: (EU4Info g, Monad m) =>
    GenericScript -> PPT g m Doc
pp_script [] = return "(Nothing)"
pp_script script = imsg2doc =<< ppMany script

flagTextMaybe :: (EU4Info g, Monad m) => Text -> PPT g m (Text,Text)
flagTextMaybe = fmap (\t -> (mempty, t)) . flagText (Just EU4Country)

-- | Extract the appropriate message(s) from a script.
ppMany :: (EU4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppMany scr = indentUp (concat <$> mapM ppOne scr)

-- | Table of handlers for statements. Dispatch on strings is /much/ quicker
-- using a lookup table than a huge @case@ expression, which uses @('==')@ on
-- each one in turn.
--
-- When adding a new statement handler, add it to one of the sections in
-- alphabetical order if possible, and use one of the generic functions for it
-- if applicable.
ppHandlers :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
ppHandlers = foldl' Tr.unionL Tr.empty
    [ handlersRhsIrrelevant
    , handlersNumeric
    , handlersNumericIcons
    , handlersModifiers
    , handlersCompound
    , handlersLocRhs
    , handlersProvince
    , handlersFlagOrProvince
    , handlersNumericOrFlag 
    , handlersAdvisorId
    , handlersTypewriter
    , handlersSimpleIcon
    , handlersSimpleFlag
    , handlersFlagOrYesNo
    , handlersIconFlagOrPronoun
    , handlersTagOrProvince -- merge?
    , handlersYesNo
    , handlersNumericOrTag
    , handlersSignedNumeric
    , handlersNumProvinces
    , handlersTextValue
    , handlersTextAtom
    , handlersSpecialComplex
    , handlersRebels
    , handlersIdeaGroups
    , handlersMisc
    , handlersIgnored
    ]

-- | Handlers for statements where RHS is irrelevant (usually "yes")
handlersRhsIrrelevant :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersRhsIrrelevant = Tr.fromList
        [("add_cardinal"           , rhsAlwaysYes MsgAddCardinal)
        ,("add_estate_burghers_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_burghers_loyalty_effect"))
        ,("add_estate_church_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_church_loyalty_effect"))
        ,("add_estate_cossacks_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|cossacks"))
        ,("add_estate_dhimmi_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_dhimmi_loyalty_effect"))
        ,("add_estate_jains_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|jains"))
        ,("add_estate_nobles_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_nobles_loyalty_effect"))
        ,("add_innovativeness_big_effect", rhsAlwaysYes MsgAddInnovativenessBigEffect)
        ,("add_innovativeness_small_effect", rhsAlwaysYes MsgAddInnovativenessSmallEffect)
        ,("add_loot_from_province_effect", rhsAlwaysYes MsgAddLootFromProvinceEffect)
        ,("add_mandate_effect"     , rhsAlwaysYes MsgAddMandateEffect)
        ,("add_mandate_large_effect", rhsAlwaysYes MsgAddMandateLargeEffect)
        ,("add_meritocracy_effect" , rhsAlwaysYes MsgAddMeritocracyEffect)
        ,("add_meritocracy_large_effect", rhsAlwaysYes MsgAddMeritocracyLargeEffect)
        ,("add_reform_progress_medium_effect", rhsAlwaysYes MsgAddReformProgressMediumEffect)
        ,("add_reform_progress_small_effect", rhsAlwaysYes MsgAddReformProgressSmallEffect)
        ,("add_stability_or_adm_power", rhsAlwaysYes MsgAddStabilityOrAdm)
        ,("boost_bureaucrats_effect", rhsAlwaysYes MsgBoostBureaucratsEffect)
        ,("boost_bureaucrats_large_effect", rhsAlwaysYes MsgBoostBureaucratsLargeEffect)
        ,("boost_eunuchs_effect", rhsAlwaysYes MsgBoostEunuchsEffect)
        ,("boost_eunuchs_large_effect", rhsAlwaysYes MsgBoostEunuchsLargeEffect)
        ,("boost_temples_effect", rhsAlwaysYes MsgBoostTemplesEffect)
        ,("boost_temples_large_effect", rhsAlwaysYes MsgBoostTemplesLargeEffect)
        ,("cancel_construction"    , rhsAlwaysYes MsgCancelConstruction) -- Canals
        ,("cb_on_overseas"         , rhsAlwaysYes MsgGainOverseasCB) -- Full Expansion
        ,("cb_on_primitives"       , rhsAlwaysYes MsgGainPrimitivesCB) -- Full Exploration
        ,("cb_on_religious_enemies", rhsAlwaysYes MsgGainReligiousCB) -- Deus Vult
        ,("check_if_non_state_advisor_effect", const $ msgToPP MsgCheckIfNonStateAdvisorEffect) -- Ignore actual percentages
        ,("divorce_consort_effect", rhsAlwaysYes MsgDivorceConsortEffect)
        ,("enable_hre_leagues"     , rhsAlwaysYes MsgEnableHRELeagues)
        ,("erase_advisor_flags_effect", rhsAlwaysYes MsgEnableHRELeagues)
        ,("increase_heir_adm_effect", rhsAlwaysYes MsgIncreaseHeirAdmEffect)
        ,("increase_heir_dip_effect", rhsAlwaysYes MsgIncreaseHeirDipEffect)
        ,("increase_heir_mil_effect", rhsAlwaysYes MsgIncreaseHeirMilEffect)
        ,("increase_legitimacy_medium_effect", rhsAlwaysYes MsgIncreaseLegitimacyMediumEffect)
        ,("increase_legitimacy_small_effect", rhsAlwaysYes MsgIncreaseLegitimacySmallEffect)
        ,("is_janissary_modifier"  , rhsAlwaysYes MsgIsJanissaryMod)
        ,("kill_heir"              , rhsAlwaysYes MsgHeirDies)
        ,("kill_ruler"             , rhsAlwaysYes MsgRulerDies)
        ,("may_agitate_for_liberty", rhsAlwaysYes MsgMayAgitateForLiberty) -- Espionage: Destabilizing Efforts
        ,("may_explore"            , rhsAlwaysYes MsgMayExplore) -- Exploration: Quest for the New World
        ,("may_infiltrate_administration", rhsAlwaysYes MsgMayInfiltrateAdministration) -- Espionage: Espionage
        ,("may_sabotage_reputation", rhsAlwaysYes MsgMaySabotageReputation) -- Espionage: Rumormongering
        ,("may_sow_discontent"     , rhsAlwaysYes MsgMaySowDiscontent) -- Espionage: Destabilizing Efforts
        ,("may_study_technology"   , rhsAlwaysYes MsgMayStudyTech) -- Espionage: Shady Recruitment
        ,("move_capital_effect"    , rhsAlwaysYes MsgMoveCapitalEffect)
        ,("set_hre_religion_treaty", rhsAlwaysYes MsgSignWestphalia)
        ,("reduced_stab_impacts"   , rhsAlwaysYes MsgReducedStabImpacts) -- Full Diplomacy
        ,("reduce_estate_burghers_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "reduce_estate_burghers_loyalty_effect"))
        ,("reduce_estate_church_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "reduce_estate_church_loyalty_effect"))
        ,("reduce_estate_cossacks_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|cossacks|lose"))
        ,("reduce_estate_dhimmi_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "reduce_estate_dhimmi_loyalty_effect"))
        ,("reduce_estate_jains_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|jains|lose"))
        ,("reduce_estate_nobles_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "reduce_estate_nobles_loyalty_effect"))
        ,("reduce_bureaucrats_effect", rhsAlwaysYes MsgReduceBureaucratsEffect)
        ,("reduce_eunuchs_effect"  , rhsAlwaysYes MsgReduceEunuchsEffect)
        ,("reduce_innovativeness_small_effect", rhsAlwaysYes MsgAddInnovativenessSmallEffect)
        ,("reduce_temples_effect"  , rhsAlwaysYes MsgReduceTemplesEffect)
        ,("reduce_legitimacy_medium_effect", rhsAlwaysYes MsgReduceLegitimacyEffect)
        ,("reduce_legitimacy_small_effect", rhsAlwaysYes MsgReduceLegitimacySmallEffect)
        ,("reduce_mandate_effect", rhsAlwaysYes MsgReduceMandateEffect)
        ,("reduce_mandate_large_effect", rhsAlwaysYes MsgReduceMandateLargeEffect)
        ,("reduce_meritocracy_effect", rhsAlwaysYes MsgReduceMeritocracyEffect)
        ,("reduce_meritocracy_large_effect", rhsAlwaysYes MsgReduceMeritocracyLargeEffect)
        ,("reduce_reform_progress_small_effect", rhsAlwaysYes MsgReduceReformProgressSmallEffect)
        ,("reduce_reform_progress_medium_effect", rhsAlwaysYes MsgReduceReformProgressMediumEffect)
        ,("reduce_reform_progress_big_effect", rhsAlwaysYes MsgReduceReformProgressBigEffect)
        ,("remove_advisor_adm_effect", rhsAlwaysYes MsgRemoveAdvisorAdmEffect) -- "The currently employed administrative advisor leaves the country's court."
        ,("remove_cardinal"        , rhsAlwaysYes MsgLoseCardinal)
        ,("remove_non_electors_emperors_from_empire_effect", rhsAlwaysYes MsgLeaveHRE)
        ,("sea_repair"             , rhsAlwaysYes MsgGainSeaRepair) -- Full Maritime
        ,("swap_non_generic_missions" , rhsAlwaysYes MsgGainNewMissions)
        ,("type" , rhsAlways "all" MsgTypeAll) -- FIXME: This is a hack to handle xxxx_area = { type = all }
        ,("auto_explore_adjacent_to_colony", rhsAlwaysYes MsgAutoExploreAdjacentToColony)
        ,("can_fabricate_for_vassals", rhsAlwaysYes MsgCanFabricateForVassals)
        ,("idea_claim_colonies"    , rhsAlwaysYes MsgIdeaClaimColonies)
        ,("may_establish_frontier" , rhsAlwaysYes MsgMayEstablishFrontier)
        ,("may_perform_slave_raid" , rhsAlwaysYes MsgMayPerformSlaveRaid)
        ,("may_perform_slave_raid_on_same_religion", rhsAlwaysYes MsgMayPerformSlaveRaidOnSameReligion)
        ,("may_recruit_female_generals", rhsAlwaysYes MsgMayRecruitFemaleGenerals)
        ,("no_religion_penalty"    , rhsAlwaysYes MsgNoReligionPenalty)
        ,("map_setup"              , rhsAlways "map_setup_random" MsgMapSetupRandom) -- In 1.30.6 all uses are "map_setup = map_setup_random"
        ]

-- | Handlers for numeric statements
handlersNumeric :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumeric = Tr.fromList
        [("add_authority"                    , numeric MsgGainAuth) -- Inti
        ,("add_colonysize"                   , numeric MsgGainColonyPopulation)
        ,("add_construction_progress"        , numeric MsgGainConstructionProgress)
        ,("add_doom"                         , numeric MsgGainDoom)
        ,("add_harmonization_progress"       , numeric MsgGainHarmonizationProgress)
        ,("add_heir_claim"                   , numeric MsgHeirGainClaim)
        ,("add_heir_support"                 , numeric MsgGainHeirSupport) -- elective monarchy
        ,("add_isolationism"                 , numeric MsgAddIsolationism)
        ,("add_next_institution_embracement" , numeric MsgAddNextInstitutionEmbracement)
        ,("add_nationalism"                  , numeric MsgGainYearsOfSeparatism)
        ,("army_size_percentage"             , numeric MsgArmySizePc) -- Inti?
        ,("authority"                        , numeric MsgAuth) -- Inti?
        ,("change_siege"                     , numeric MsgGainSiegeProgress)
        ,("colonysize"                       , numeric MsgColonySettlers)
        ,("drill_gain_modifier"              , numeric MsgDrillGainMod)
        ,("establish_order_cost"             , numeric MsgEstablishOrderCost)
        ,("had_recent_war"                   , numeric MsgWasAtWar)
        ,("heir_age"                         , numeric MsgHeirAge)
        ,("is_year"                          , numeric MsgYearIs)
        ,("legitimacy_equivalent"            , numeric MsgLegitimacyEquivalent)
        ,("num_of_colonial_subjects"         , numeric MsgNumColonialSubjects)
        ,("num_of_colonies"                  , numeric MsgNumColonies)
        ,("num_of_loans"                     , numeric MsgNumLoans)
        ,("num_of_mercenaries"               , numeric MsgNumMercs)
        ,("num_of_ports"                     , numeric MsgNumPorts) -- same as num_of_total_ports?
        ,("num_of_rebel_armies"              , numeric MsgNumRebelArmies)
        ,("num_of_rebel_controlled_provinces", numeric MsgNumRebelControlledProvinces)
        ,("num_of_total_ports"               , numeric MsgNumPorts) -- same as num_of_ports?
        ,("num_of_trade_embargos"            , numeric MsgNumEmbargoes)
        ,("percentage_backing_issue"         , numeric MsgPctBackingParliamentIssue)
        ,("revolt_percentage"                , numeric MsgRevoltPercentage)
        ,("ruler_age"                        , numeric MsgRulerAge)
        ,("trade_income_percentage"          , numeric MsgTradeIncomePercentage)
        ,("units_in_province"                , numeric MsgUnitsInProvince)
        -- Special cases
        ,("legitimacy_or_horde_unity"        , numeric MsgLegitimacyOrHordeUnity)
        ]

-- | Handlers for numeric statements with icons
handlersNumericIcons :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericIcons = Tr.fromList
        [("absolutism"               , numericIcon "absolutism" MsgAbsolutism)
        ,("add_absolutism"           , numericIcon "absolutism" MsgGainAbsolutism)
        ,("add_adm_power"            , numericIcon "adm" MsgGainADM)
        ,("add_army_tradition"       , numericIcon "army tradition" MsgGainAT)
        ,("add_army_professionalism" , numericIcon "army professionalism" MsgGainArmyProfessionalism)
        ,("add_base_manpower"        , numericIcon "manpower" MsgGainBM)
        ,("add_base_production"      , numericIcon "production" MsgGainBP)
        ,("add_base_tax"             , numericIcon "base tax" MsgGainBT)
        ,("add_center_of_trade_level", numericIcon "center of trade" MsgAddCOTLevel)
        ,("add_church_power"         , numericIcon "church power" MsgGainChurchPower)
        ,("add_corruption"           , numericIcon "corruption" MsgGainCorruption)
        ,("add_devastation"          , numericIcon "devastation" MsgGainDevastation)
        ,("add_devotion"             , numericIcon "devotion" MsgGainDevotion)
        ,("add_dip_power"            , numericIcon "dip" MsgGainDIP)
        ,("add_fervor"               , numericIcon "monthly fervor" MsgGainFervor)
        ,("add_harmony"              , numericIcon "yearly harmony" MsgGainHarmony)
        ,("add_horde_unity"          , numericIcon "horde unity" MsgGainHordeUnity)
        ,("add_imperial_influence"   , numericIcon "imperial authority" MsgGainImperialAuthority)
        ,("add_inflation"            , numericIcon "inflation" MsgGainInflation)
        ,("add_karma"                , numericIcon "high karma" MsgGainKarma)
        ,("add_legitimacy"           , numericIcon "legitimacy" MsgGainLegitimacy)
        ,("add_liberty_desire"       , numericIcon "liberty desire" MsgGainLibertyDesire)
        ,("add_local_autonomy"       , numericIcon "local autonomy" MsgGainLocalAutonomy)
        ,("add_mandate"              , numericIcon "mandate" MsgGainMandate)
        ,("add_mercantilism"         , numericIcon "mercantilism" MsgGainMercantilism)
        ,("add_mil_power"            , numericIcon "mil" MsgGainMIL)
        ,("add_militarised_society"  , numericIcon "militarization of state" MsgGainMilitarization)
        ,("add_navy_tradition"       , numericIcon "navy tradition" MsgGainNavyTradition)
        ,("add_papal_influence"      , numericIcon "papal influence" MsgGainPapalInfluence)
        ,("add_patriarch_authority"  , numericIcon "patriarch authority" MsgGainPatAuth)
        ,("add_piety"                , numericIconChange "mysticism" "legalism" MsgGainMysticism MsgGainLegalism)
        ,("add_prestige"             , numericIcon "prestige" MsgGainPrestige)
        ,("add_prosperity"           , numericIcon "prosperity" MsgGainProsperity)
        ,("add_reform_desire"        , numericIcon "reform desire" MsgGainReformDesire)
        ,("add_republican_tradition" , numericIcon "republican tradition" MsgGainRepTrad)
        ,("add_revolutionary_zeal"   , numericIcon "revolutionary zeal" MsgGainRevolutionaryZeal)
        ,("add_stability"            , numericIcon "stability" MsgGainStability)
        ,("add_splendor"             , numericIcon "splendor" MsgGainSplendor)
        ,("add_tariff_value"         , numericIcon "gloabl tariffs" MsgAddTariffValue)
        ,("add_treasury"             , numericIcon "ducats" MsgAddTreasury)
        ,("add_tribal_allegiance"    , numericIcon "tribal allegiance" MsgGainTribalAllegiance)
        ,("add_unrest"               , numericIcon "local unrest" MsgAddLocalUnrest)
        ,("add_war_exhaustion"       , numericIcon "war exhaustion" MsgGainWarExhaustion)
        ,("add_yearly_manpower"      , numericIcon "manpower" MsgGainYearlyManpower)
        ,("add_yearly_sailors"       , numericIcon "sailors" MsgGainYearlySailors)
        ,("add_years_of_income"      , numericIcon "ducats" MsgAddYearsOfIncome)
        ,("adm"                      , numericIcon "adm" MsgRulerADM)
        ,("administrative_efficiency", numericIconBonus "administrative efficiency" MsgAdminEfficiency MsgAdminEfficiencyBonus)
        ,("adm_power"                , numericIcon "adm" MsgHasADM)
        ,("army_organiser"           , numericIconLoc "army organizer" "army_organiser" MsgHasAdvisorLevel)
        ,("army_reformer"            , numericIconLoc "army reformer" "army_reformer" MsgHasAdvisorLevel)
        ,("army_tradition"           , numericIconBonus "army tradition" MsgArmyTradition MsgYearlyArmyTradition)
        ,("artist"                   , numericIconLoc "artist" "artist" MsgHasAdvisorLevel)
        ,("base_manpower"            , numericIcon "manpower" MsgBaseManpower)
        ,("base_production"          , numericIcon "base production" MsgBaseProduction)
        ,("base_tax"                 , numericIcon "base tax" MsgBaseTax)
        ,("blockade"                 , numericIcon "blockade" MsgBlockade)
        ,("change_adm"               , numericIcon "adm" MsgGainADMSkill)
        ,("change_dip"               , numericIcon "dip" MsgGainDIPSkill)
        ,("change_mil"               , numericIcon "mil" MsgGainMILSkill)
        ,("change_statists_vs_orangists", numericIconChange "republic" "monarchy" MsgStrengthenStatists MsgStrengthenOrangists)
        ,("colonial_governor"        , numericIconLoc "colonial governor" "colonial_governor" MsgHasAdvisorLevel)
        ,("colonist_placement_chance", numericIcon "settler chance" MsgSettlerChance)
        ,("commandant"               , numericIconLoc "commandant" "commandant" MsgHasAdvisorLevel)
        ,("consort_adm"              , numericIcon "adm" MsgConsortADM)
        ,("consort_dip"              , numericIcon "dip" MsgConsortDIP)
        ,("consort_mil"              , numericIcon "mil" MsgConsortMIL)
        ,("corruption"               , numericIcon "corruption" MsgCorruption)
        ,("create_admiral"           , numericIcon "admiral" MsgCreateAdmiral)
        ,("create_conquistador"      , numericIcon "conquistador" MsgCreateConquistador)
        ,("create_explorer"          , numericIcon "explorer" MsgCreateExplorer)
        ,("create_general"           , numericIcon "general" MsgCreateGeneral)
        ,("devastation"              , numericIcon "devastation" MsgDevastation)
        ,("development"              , numericIcon "development" MsgDevelopment)
        ,("development_cost"         , numericIcon "development cost" MsgDevelCost)
        ,("dip"                      , numericIcon "dip" MsgRulerDIP)
        ,("dip_power"                , numericIcon "adm" MsgHasDIP)
        ,("dip_tech"                 , numericIcon "dip tech" MsgDIPTech)
        ,("diplomat"                 , numericIconLoc "diplomat" "diplomat" MsgHasAdvisorLevel)
        ,("embracement_cost"         , numericIcon "embracement cost" MsgEmbracementCost)
        ,("fire_damage"              , numericIcon "land fire damage" MsgLandFireDamage)
        ,("fort_level"               , numericIcon "fort level" MsgFortLevel)
        ,("global_foreign_trade_power", numericIcon "trade power abroad" MsgTradePowerAbroad)
        ,("global_institution_spread", numericIcon "institution spread" MsgInstitutionSpread)
        ,("global_own_trade_power"   , numericIcon "domestic trade power" MsgDomesticTradePower)
        ,("global_prov_trade_power_modifier", numericIcon "provincial trade power modifier" MsgProvTradePowerMod)
        ,("global_tax_income"        , numericIcon "tax income" MsgGlobalTaxIncome)
        ,("gold_income_percentage"   , numericIcon "gold" MsgGoldIncomePercentage)
        ,("heavy_ship_cost"          , numericIcon "heavy ship cost" MsgHeavyShipCost)
        ,("heir_chance"              , numericIcon "chance of new heir" MsgHeirChance)
        ,("heir_adm"                 , numericIcon "adm" MsgHeirADM)
        ,("heir_dip"                 , numericIcon "dip" MsgHeirDIP)
        ,("heir_mil"                 , numericIcon "mil" MsgHeirMIL)
        ,("horde_unity"              , numericIconBonus "horde unity" MsgHordeUnity MsgYearlyHordeUnity)
        ,("imperial_influence"       , numericIcon "imperial authority" MsgImperialAuthority)
        ,("imperial_mandate"         , numericIconBonus "mandate growth modifier" MsgImperialMandate MsgImperialMandateGrowth)
        ,("inflation"                , numericIcon "inflation" MsgInflation)
        ,("inquisitor"               , numericIconLoc "inquisitor" "inquisitor" MsgHasAdvisorLevel)
        ,("karma"                    , numericIcon "high karma" MsgKarma)
        ,("legitimacy"               , numericIconBonus "legitimacy" MsgLegitimacy MsgYearlyLegitimacy)
        ,("liberty_desire"           , numericIcon "liberty desire" MsgLibertyDesire)
        ,("local_autonomy"           , numericIconBonus "local autonomy" MsgLocalAutonomy MsgMonthlylAutonomyChange)
        ,("local_build_cost"         , numericIcon "local construction cost" MsgLocalConstructionCost)
        ,("local_colonial_growth"    , numericIcon "local settler increase" MsgLocalSettlerIncrease)
        ,("local_culture_conversion_cost", numericIcon "local culture conversion cost" MsgLocalCultureConversionCost)
        ,("local_defensiveness"      , numericIcon "local defensiveness" MsgLocalDefensiveness)
        ,("local_development_cost"   , numericIcon "local development cost" MsgLocalDevelopmentCost )
        ,("local_hostile_movement_speed", numericIcon "local hostile movement speed" MsgLocalHostileMovementSpeed)
        ,("local_institution_spread" , numericIcon "local institution spread" MsgLocalInstitutionSpread)
        ,("local_manpower_modifier"  , numericIcon "local manpower modifier" MsgLocalManpowerMod)
        ,("local_missionary_strength", numericIcon "local missionary strength" MsgLocalMissionaryStrength)
        ,("local_monthly_devastation", numericIcon "local monthly devastation" MsgLocalMonthlyDevastation)
        ,("local_production_efficiency", numericIcon "local production efficiency" MsgLocalProdEff)
        ,("local_state_maintenance_modifier", numericIcon "local state maintenance modifier" MsgLocalStateMaintMod)
        ,("local_tax_modifier"       , numericIcon "local tax modifier" MsgLocalTaxMod)
        ,("local_unrest"             , numericIcon "local unrest" MsgLocalUnrest)
        ,("manpower"                 , numericIcon "manpower" MsgManpower)
        ,("manpower_percentage"      , numericIcon "manpower" MsgManpowerPercentage)
        ,("max_absolutism"           , numericIcon "max absolutism" MsgMaxAbsolutism)
        ,("mercantilism"             , numericIcon "mercantilism" MsgMercantilism)
        ,("mercenary_discipline"     , numericIcon "mercenary discipline" MsgMercenaryDiscipline)
        ,("meritocracy"              , numericIconBonus "meritocracy" MsgMeritocracy MsgYearlyMeritocracy)
        ,("mil"                      , numericIcon "mil" MsgRulerMIL)
        ,("mil_power"                , numericIcon "adm" MsgHasMIL)
        ,("mil_tech"                 , numericIcon "mil tech" MsgMILTech)
        ,("monthly_income"           , numericIcon "ducats" MsgMonthlyIncome)
        ,("mr_aristocrats_influence" , numericIcon "aristocrats influence" MsgAristocratsInfluence)
        ,("nationalism"              , numericIcon "years of separatism" MsgSeparatism)
        ,("natural_scientist"        , numericIconLoc "natural scientist" "natural_scientist" MsgHasAdvisorLevel)
        ,("naval_forcelimit"         , numericIcon "naval force limit" MsgNavalForcelimit)
        ,("naval_reformer"           , numericIconLoc "naval reformer" "naval_reformer" MsgHasAdvisorLevel)
        ,("navigator"                , numericIconLoc "navigator" "navigator" MsgHasAdvisorLevel)
        ,("navy_tradition"           , numericIconBonus "navy tradition" MsgNavyTradition MsgYearlyNavyTradition)
        ,("navy_reformer"            , numericIconLoc "naval reformer"  "naval_reformer"MsgHasAdvisorLevel) -- both are used
        ,("navy_size_percentage"     , numericIcon "naval force limit" MsgNavyPercentage)
        ,("num_accepted_cultures"    , numericIcon "max promoted cultures" MsgMaxPromotedCultures)
        ,("num_of_allies"            , numericIcon "alliance" MsgNumAllies)
        ,("num_of_cardinals"         , numericIcon "cardinal" MsgNumCardinals)
        ,("num_of_colonists"         , numericIcon "colonists" MsgNumColonists)
        ,("num_of_heavy_ship"        , numericIcon "heavy ship" MsgNumHeavyShips)
        ,("num_of_light_ship"        , numericIcon "light ship" MsgNumLightShips)
        ,("num_of_merchants"         , numericIcon "merchant" MsgNumMerchants)
        ,("num_of_missionaries"      , numericIcon "missionary" MsgNumMissionaries)
        ,("num_of_royal_marriages"   , numericIcon "royal marriage" MsgNumRoyalMarriages)
        ,("num_of_unions"            , numericIcon "personal union" MsgNumUnions)
        ,("num_of_vassals"           , numericIcon "vassal" MsgNumVassals) -- includes other subjects?
        ,("overextension_percentage" , numericIcon "overextension" MsgOverextension)
        ,("philosopher"              , numericIconLoc "philosopher" "philosopher" MsgHasAdvisorLevel)
        ,("province_trade_power_modifier", numericIcon "local trade power modifier" MsgLocalTradePowerMod)
        ,("province_trade_power_value", numericIcon "local trade power" MsgLocalTradePower)
        ,("quartermaster"            , numericIconLoc "quartermaster" "quartermaster" MsgHasAdvisorLevel)
        ,("raze_power_gain"          , numericIcon "razing power gain" MsgRazingPowerGain)
        ,("recover_navy_morale_speed", numericIcon "recover navy morale speed" MsgRecoverNavyMoraleSpeed)
        ,("recruitmaster"            , numericIconLoc "recruitmaster" "recruitmaster" MsgHasAdvisorLevel)
        ,("province_trade_power"     , numericIcon "trade power" MsgProvinceTradePower)
        -- the number for the following is negated compared to the displayed figure
        ,("reduced_liberty_desire"   , numericIcon "liberty desire in subjects" MsgSubjectLibertyDesire)
        ,("reform_desire"            , numericIcon "reform desire" MsgReformDesire)
        ,("regiment_recruit_speed"   , numericIcon "recruitment time" MsgRecruitmentTime)
        ,("religious_unity"          , numericIconBonus "religious unity" MsgReligiousUnity MsgReligiousUnityBonus)
        ,("republican_tradition"     , numericIconBonus "republican tradition" MsgRepTrad MsgYearlyRepTrad)
        ,("sailors_percentage"       , numericIcon "sailors" MsgSailorsPercentage)
        ,("ship_recruit_speed"       , numericIcon "shipbuilding time" MsgShipbuildingTime)
        ,("shock_damage"             , numericIcon "shock damage" MsgShockDamage)
        ,("stability"                , numericIcon "stability" MsgStability)
        ,("statesman"                , numericIconLoc "statesman" "statesman" MsgHasAdvisorLevel)
        ,("state_maintenance_modifier", numericIcon "state maintenance" MsgStateMaintMod)
        ,("tax_income"               , numericIcon "tax income" MsgTaxIncome)
        ,("theologian"               , numericIconLoc "theologian" "theologian" MsgHasAdvisorLevel)
        ,("total_development"        , numericIcon "development" MsgTotalDevelopment)
        ,("total_number_of_cardinals", numericIcon "cardinal" MsgTotalCardinals) -- in the world
        ,("trade_efficiency"         , numericIconBonus "trade efficiency" MsgTradeEfficiency MsgTradeEfficiencyBonus)
        ,("trade_goods_size"         , numericIcon "local goods produced" MsgLocalGoodsProduced)
        ,("trade_goods_size_modifier", numericIcon "local goods produced modifier" MsgLocalGoodsProducedMod)
        ,("trade_value_modifier"     , numericIcon "trade value modifier" MsgTradeValueMod)
        ,("trader"                   , numericIconLoc "trader" "trader" MsgHasAdvisorLevel)
        ,("treasury"                 , numericIcon "ducats" MsgHasDucats)
        ,("tribal_allegiance"        , numericIconBonus "tribal allegiance" MsgTribalAllegiance MsgTribalAllegianceBonus)
        ,("unrest"                   , numericIcon "unrest" MsgUnrest)
        ,("war_exhaustion"           , numericIconBonus "war exhaustion" MsgWarExhaustion MsgMonthlyWarExhaustion)
        ,("war_score"                , numericIcon "war score" MsgWarScore)
        ,("yearly_absolutism"        , numericIcon "absolutism" MsgYearlyAbsolutism)
        ,("yearly_army_professionalism", numericIcon "yearly army professionalism" MsgYearlyArmyProfessionalism)
        ,("yearly_corruption"        , numericIcon "yearly corruption" MsgYearlyCorruption)
        ,("yearly_tribal_allegiance" , numericIcon "tribal allegiance" MsgTribalAllegianceBonus)
        ,("years_of_income"          , numericIcon "ducats" MsgYearsOfIncome)
        -- Used in ideas and other bonuses, omit "gain/lose" in l10n
        ,("accepted_culture_threshold"        , numericIcon "accepted culture threshold" MsgAccCultureThreshold)
        ,("adm_tech_cost_modifier"            , numericIcon "adm tech cost" MsgADMTechCost)
        ,("advisor_cost"                      , numericIcon "advisor cost" MsgAdvisorCost)
        ,("advisor_pool"                      , numericIcon "advisor pool" MsgPossibleAdvisors)
        ,("ae_impact"                         , numericIcon "ae impact" MsgAEImpact)
        ,("army_tradition_decay"              , numericIcon "army tradition decay" MsgArmyTraditionDecay)
        ,("artillery_cost"                    , numericIcon "artillery cost" MsgArtilleryCost)
        ,("artillery_power"                   , numericIcon "artillery power" MsgArtilleryCombatAbility)
        ,("blockade_efficiency"               , numericIcon "blockade efficiency" MsgBlockadeEfficiency)
        ,("build_cost"                        , numericIcon "build cost" MsgBuildCost)
        ,("caravan_power"                     , numericIcon "caravan power" MsgCaravanPower)
        ,("cavalry_cost"                      , numericIcon "cavalry cost" MsgCavalryCost)
        ,("cavalry_power"                     , numericIcon "cavalry power" MsgCavalryCombatAbility)
        ,("church_power_modifier"             , numericIcon "church power" MsgChurchPowerModifier)
        ,("colonists"                         , numericIcon "colonists" MsgColonists)
        ,("core_creation"                     , numericIcon "core creation cost" MsgCoreCreationCost)
        ,("culture_conversion_cost"           , numericIcon "culture conversion cost" MsgCultureConvCost)
        ,("defensiveness"                     , numericIcon "defensiveness" MsgFortDefense)
        ,("devotion"                          , numericIcon "devotion" MsgYearlyDevotion)
        ,("diplomatic_annexation_cost"        , numericIcon "diplomatic annexation cost" MsgDiploAnnexCost)
        ,("diplomatic_reputation"             , numericIcon "diplomatic reputation" MsgDiploRep)
        ,("diplomatic_upkeep"                 , numericIcon "diplomatic upkeep" MsgDiploRelations)
        ,("diplomats"                         , numericIcon "diplomats" MsgDiplomats)
        ,("dip_tech_cost_modifier"            , numericIcon "dip tech cost" MsgDIPTechCost)
        ,("discipline"                        , numericIcon "discipline" MsgDiscipline)
        ,("discovered_relations_impact"       , numericIcon "discovered relations impact" MsgCovertActionRelationImpact)
        ,("embargo_efficiency"                , numericIcon "embargo efficiency" MsgEmbargoEff)
        ,("enemy_core_creation"               , numericIcon "enemy core creation" MsgHostileCoreCreation)
        ,("envoy_travel_time"                 , numericIcon "envoy travel time" MsgEnvoyTravelTime)
        ,("fabricate_claims_cost"             , numericIcon "cost to fabricate claims" MsgCostToFabricateClaims)
        ,("fabricate_claims_time"             , numericIcon "time to fabricate claims" MsgTimeToFabricateClaims)
        ,("fort_maintenance_modifier"         , numericIcon "fort maintenance" MsgFortMaintenance)
        ,("free_leader_pool"                  , numericIcon "free leader pool" MsgLeadersWithoutUpkeep)
        ,("galley_power"                      , numericIcon "galley power" MsgGalleyCombatAbility)
        ,("garrison_size"                     , numericIcon "garrison size" MsgGarrisonSize)
        ,("global_autonomy"                   , numericIcon "global autonomy" MsgGlobalAutonomy)
        ,("global_colonial_growth"            , numericIcon "global settler increase" MsgGlobalSettlers)
        ,("global_heretic_missionary_strength", numericIcon "global heretic missionary strength" MsgMissionaryStrengthVsHeretics)
        ,("global_manpower_modifier"          , numericIcon "national manpower modifier" MsgNationalManpowerMod)
        ,("global_missionary_strength"        , numericIcon "missionary strength" MsgMissionaryStrength)
        ,("global_regiment_cost"              , numericIcon "regiment cost" MsgRegimentCost)
        ,("global_regiment_recruit_speed" {-sic-}, numericIcon "global regiment recruit speed" MsgRecruitmentTime)
        ,("global_ship_cost"                  , numericIcon "ship cost" MsgGlobalShipCost)
        ,("global_ship_recruit_speed" {- sic -}, numericIcon "shipbuilding time" MsgShipbuildingTime)
        ,("global_ship_repair"                , numericIcon "global ship repair" MsgGlobalShipRepair)
        ,("global_spy_defence"                , numericIcon "global spy defence" MsgGlobalSpyDefence)
        ,("global_tariffs"                    , numericIcon "global tariffs" MsgGlobalTariffs)
        ,("global_tax_modifier"               , numericIcon "global tax modifier" MsgGlobalTaxModifier)
        ,("global_trade_goods_size_modifier"  , numericIcon "goods produced modifier" MsgGoodsProducedMod)
        ,("global_trade_power"                , numericIcon "global trade power" MsgGlobalTradePower)
        ,("global_unrest"                     , numericIcon "national unrest" MsgNationalUnrest)
        ,("heavy_ship_power"                  , numericIcon "heavy ship power" MsgHeavyShipCombatAbility)
        ,("hostile_attrition"                 , numericIcon "attrition for enemies" MsgAttritionForEnemies)
        ,("idea_cost"                         , numericIcon "idea cost" MsgIdeaCost)
        ,("improve_relation_modifier"         , numericIcon "improve relations" MsgImproveRelations)
        ,("infantry_cost"                     , numericIcon "infantry cost" MsgInfantryCost)
        ,("infantry_power"                    , numericIcon "infantry power" MsgInfantryCombatAbility)
        ,("inflation_action_cost"             , numericIcon "reduce inflation cost" MsgReduceInflationCost)
        ,("inflation_reduction"               , numericIcon "inflation reduction" MsgYearlyInflationReduction)
        ,("interest"                          , numericIcon "interest" MsgInterestPerAnnum)
        ,("land_maintenance_modifier"         , numericIcon "land maintenance" MsgLandMaintenanceMod)
        ,("land_morale"                       , numericIcon "morale of armies" MsgMoraleOfArmies)
        ,("land_attrition"                    , numericIcon "land attrition" MsgLandAttrition)
        ,("land_forcelimit_modifier"          , numericIcon "land forcelimit modifier" MsgLandForcelimitMod)
        ,("leader_land_fire"                  , numericIcon "land leader fire" MsgGainLandLeaderFire)
        ,("leader_land_shock"                 , numericIcon "land leader shock" MsgGainLandLeaderShock)
        ,("leader_land_manuever" {- sic -}    , numericIcon "land leader maneuver" MsgGainLandLeaderManeuver)
        ,("leader_land_siege"                 , numericIcon "leader siege" MsgGainLandLeaderSiege)
        ,("leader_naval_fire"                 , numericIcon "naval leader fire" MsgGainNavalLeaderFire)
        ,("leader_naval_manuever" {- sic -}   , numericIcon "naval leader maneuver" MsgGainNavalLeaderManeuver)
        ,("leader_naval_shock"                , numericIcon "naval leader shock" MsgGainNavalLeaderShock)
        ,("light_ship_power"                  , numericIcon "light ship power" MsgLightShipCombatAbility)
        ,("manpower_recovery_speed"           , numericIcon "manpower recovery speed" MsgManpowerRecoverySpeed)
        ,("mercenary_cost"                    , numericIcon "mercenary cost" MsgMercCost)
        ,("merc_maintenance_modifier"         , numericIcon "merc maintenance modifier" MsgMercMaintenance)
        ,("merchants"                         , numericIcon "merchants" MsgMerchants)
        ,("mil_tech_cost_modifier"            , numericIcon "mil tech cost" MsgMILTechCost)
        ,("missionaries"                      , numericIcon "missionaries" MsgMissionaries)
        ,("missionary_maintenance_cost"       , numericIcon "missionary maintenance cost" MsgMissionaryMaintenanceCost)
        ,("monthly_fervor_increase"           , numericIcon "monthly fervor" MsgMonthlyFervor)
        ,("movement_speed"                    , numericIcon "movement speed" MsgMovementSpeed)
        ,("naval_attrition"                   , numericIcon "naval attrition" MsgNavalAttrition)
        ,("naval_forcelimit_modifier"         , numericIcon "naval forcelimit" MsgNavalForcelimitMod)
        ,("naval_maintenance_modifier"        , numericIcon "naval maintenance" MsgNavalMaintenanceMod)
        ,("naval_morale"                      , numericIcon "morale of navies" MsgMoraleOfNavies)
        ,("navy_tradition"                    , numericIcon "navy tradition" MsgYearlyNavyTradition)
        ,("navy_tradition_decay"              , numericIcon "navy tradition decay" MsgNavyTraditionDecay)
        ,("papal_influence"                   , numericIconBonus "papal influence" MsgPapalInfluence MsgYearlyPapalInfluence)
        ,("possible_mercenaries"              , numericIcon "available mercenaries" MsgAvailableMercs)
        ,("prestige"                          , numericIconBonus "prestige" MsgPrestige MsgYearlyPrestige)
        ,("prestige_decay"                    , numericIcon "prestige decay" MsgPrestigeDecay)
        ,("prestige_from_land"                , numericIcon "prestige from land" MsgPrestigeFromLand)
        ,("prestige_from_naval"               , numericIcon "prestige from naval" MsgPrestigeFromNaval)
        ,("privateer_efficiency"              , numericIcon "privateer efficiency" MsgPrivateerEff)
        ,("production_efficiency"             , numericIconBonus "production efficiency" MsgProdEff MsgProdEffBonus)
        ,("province_warscore_cost"            , numericIcon "province warscore cost" MsgProvinceWarscoreCost)
        ,("rebel_support_efficiency"          , numericIcon "reform desire" MsgRebelSupportEff)
        ,("recover_army_morale_speed"         , numericIcon "recover army morale speed" MsgRecoverArmyMoraleSpeed)
        ,("reinforce_speed"                   , numericIcon "reinforce speed" MsgReinforceSpeed)
        ,("relations_decay_of_me"             , numericIcon "better relations over time" MsgBetterRelationsOverTime)
        ,("ship_durability"                   , numericIcon "ship durability" MsgShipDurability)
        ,("siege_ability"                     , numericIcon "siege ability" MsgSiegeAbility)
        ,("spy_offence"                       , numericIcon "spy offense" MsgSpyOffense) -- US spelling in game
        ,("stability_cost_modifier"           , numericIcon "stability cost" MsgStabilityCost)
        ,("technology_cost"                   , numericIcon "technology cost" MsgTechCost)
        ,("tolerance_heathen"                 , numericIcon "tolerance heathen" MsgToleranceHeathen)
        ,("tolerance_heretic"                 , numericIcon "tolerance heretic" MsgToleranceHeretic)
        ,("tolerance_own"                     , numericIcon "tolerance own" MsgToleranceTrue)
        ,("trade_range_modifier"              , numericIcon "trade range" MsgTradeRange)
        ,("trade_steering"                    , numericIcon "trade steering" MsgTradeSteering)
        ,("unjustified_demands"               , numericIcon "unjustified demands" MsgUnjustifiedDemands)
        ,("vassal_forcelimit_bonus"           , numericIcon "vassal forcelimit bonus" MsgVassalForcelimitContribution)
        ,("vassal_income"                     , numericIcon "income from vassals" MsgIncomeFromVassals)
        ,("war_exhaustion_cost"               , numericIcon "war exhaustion cost" MsgWarExhaustionCost)
        ,("years_of_nationalism"              , numericIcon "years of separatism" MsgYearsOfSeparatism)
        ,("admiral_cost"                      , numericIcon "admiral cost" MsgAdmiralCost)
        ,("allowed_marine_fraction"           , numericIcon "marines force limit" MsgAllowedMarineFraction)
        ,("allowed_num_of_buildings"          , numericIcon "possible number of buildings" MsgAllowedNumOfBuildings)
        ,("amount_of_banners"                 , numericIcon "possible manchu banners" MsgAmountOfBanners)
        ,("appoint_cardinal_cost"             , numericIcon "appoint cardinal cost" MsgAppointCardinalCost)
        ,("army_tradition_from_battle"        , numericIcon "army tradition from battles" MsgArmyTraditionFromBattle)
        ,("artillery_fire"                    , numericIcon "artillery fire" MsgArtilleryFire)
        ,("autonomy_change_time"              , numericIcon "autonomy change cooldown" MsgAutonomyChangeTime)
        ,("backrow_artillery_damage"          , numericIcon "artillery damage from back row" MsgBackrowArtilleryDamage)
        ,("brahmins_hindu_loyalty_modifier"   , numericIcon "brahmins loyalty" MsgBrahminsHinduLoyaltyModifier)
        ,("brahmins_muslim_loyalty_modifier"  , numericIcon "brahmins loyalty" MsgBrahminsMuslimLoyaltyModifier)
        ,("build_time"                        , numericIcon "construction time" MsgBuildTime)
        ,("burghers_influence_modifier"       , numericIcon "burghers influence" MsgBurghersInfluenceModifier)
        ,("burghers_loyalty_modifier"         , numericIcon "burghers loyalty" MsgBurghersLoyaltyModifier)
        ,("capture_ship_chance"               , numericIcon "chance to capture enemy ships" MsgCaptureShipChance)
        ,("cav_to_inf_ratio"                  , numericIcon "cavalry to infantry ratio" MsgCavToInfRatio)
        ,("cavalry_flanking"                  , numericIcon "cavalry flanking ability" MsgCavalryFlanking)
        ,("center_of_trade_upgrade_cost"      , numericIcon "center of trade upgrade cost" MsgCenterOfTradeUpgradeCost)
        ,("church_loyalty_modifier"           , numericIcon "clergy loyalty" MsgChurchLoyaltyModifier)
        ,("curia_treasury_contribution"       , numericIcon "curia treasury contribution" MsgCuriaTreasuryContribution)
        ,("dhimmi_loyalty_modifier"           , numericIcon "dhimmi loyalty" MsgDhimmiLoyaltyModifier)
        ,("disengagement_chance"              , numericIcon "ship disengagement chance" MsgDisengagementChance)
        ,("drill_decay_modifier"              , numericIcon "regiment drill loss" MsgDrillDecayModifier)
        ,("enforce_religion_cost"             , numericIcon "cost of enforcing religion through war" MsgEnforceReligionCost)
        ,("expel_minorities_cost"             , numericIcon "expel minorities cost" MsgExpelMinoritiesCost)
        ,("female_advisor_chance"             , numericIcon "female advisor chance" MsgFemaleAdvisorChance)
        ,("fire_damage_received"              , numericIcon "fire damage received" MsgFireDamageReceived)
        ,("flagship_cost"                     , numericIcon "flagship cost" MsgFlagshipCost)
        ,("free_adm_policy"                   , numericIcon "administrative free policies" MsgFreeAdmPolicy)
        ,("free_dip_policy"                   , numericIcon "diplomatic free policies" MsgFreeDipPolicy)
        ,("free_mil_policy"                   , numericIcon "military free policies" MsgFreeMilPolicy)
        ,("galley_cost"                       , numericIcon "galley cost" MsgGalleyCost)
        ,("general_cost"                      , numericIcon "general cost" MsgGeneralCost)
        ,("global_garrison_growth"            , numericIcon "national garrison growth" MsgGlobalGarrisonGrowth)
        ,("global_naval_engagement_modifier"  , numericIcon "global naval engagement" MsgGlobalNavalEngagementModifier)
        ,("global_religious_conversion_resistance", numericIcon "resistance to reformation" MsgGlobalReligiousConversionResistance)
        ,("global_sailors_modifier"           , numericIcon "national sailors modifier" MsgGlobalSailorsModifier)
        ,("global_ship_trade_power"           , numericIcon "ship trade power" MsgGlobalShipTradePower)
        ,("global_supply_limit_modifier"      , numericIcon "supply limit modifier" MsgGlobalSupplyLimitModifier)
        ,("governing_capacity_modifier"       , numericIcon "governing capacity modifier" MsgGoverningCapacityModifier)
        ,("harsh_treatment_cost"              , numericIcon "harsh treatment cost" MsgHarshTreatmentCost)
        ,("imperial_authority"                , numericIcon "imperial authority growth modifier" MsgImperialAuthorityGrowthModifier)
        ,("imperial_authority_value"          , numericIcon "imperial authority modifier" MsgImperialAuthorityValue)
        ,("innovativeness_gain"               , numericIcon "innovativeness gain" MsgInnovativenessGain)
        ,("justify_trade_conflict_cost"       , numericIcon "cost to justify trade conflict" MsgJustifyTradeConflictCost)
        ,("leader_siege"                      , numericIcon "leader siege" MsgLeaderSiege)
        ,("legitimate_subject_elector"        , numericIcon "legitimate subject elector" MsgLegitimateSubjectElector)
        ,("liberty_desire_from_subject_development", numericIcon "liberty desire from subjects development" MsgLibertyDesireFromSubjectDevelopment)
        ,("light_ship_cost"                   , numericIcon "light ship cost" MsgLightShipCost)
        ,("local_friendly_movement_speed"     , numericIcon "friendly movement speed" MsgLocalFriendlyMovementSpeed)
        ,("local_missionary_maintenance_cost" , numericIcon "local missionary maintenance" MsgLocalMissionaryMaintenanceCost)
        ,("local_regiment_cost"               , numericIcon "local regiment cost" MsgLocalRegimentCost)
        ,("local_religious_conversion_resistance", numericIcon "resistance to reformation" MsgLocalReligiousConversionResistance)
        ,("local_sailors_modifier"            , numericIcon "local sailors modifier" MsgLocalSailorsModifier)
        ,("local_ship_cost"                   , numericIcon "local ship cost" MsgLocalShipCost)
        ,("local_ship_repair"                 , numericIcon "local ship repair" MsgLocalShipRepair)
        ,("loot_amount"                       , numericIcon "looting speed" MsgLootAmount)
        ,("max_revolutionary_zeal"            , numericIcon "maximum revolutionary zeal" MsgMaxRevolutionaryZeal)
        ,("mercenary_manpower"                , numericIcon "mercenary manpower" MsgMercenaryManpower)
        ,("migration_cooldown"                , numericIcon "migration cooldown" MsgMigrationCooldown)
        ,("min_autonomy_in_territories"       , numericIcon "minimum autonomy in territories" MsgMinAutonomyInTerritories)
        ,("monarch_diplomatic_power"          , numericIcon "monarch diplomatic skill" MsgMonarchDiplomaticPower)
        ,("monarch_military_power"            , numericIcon "monarch military skill" MsgMonarchMilitaryPower)
        ,("monthly_piety"                     , numericIcon "monthly piety" MsgMonthlyPiety)
        ,("mr_guilds_influence"               , numericIcon "guilds influence" MsgMrGuildsInfluence)
        ,("mr_traders_influence"              , numericIcon "traders influence" MsgMrTradersInfluence)
        ,("native_assimilation"               , numericIcon "native assimilation" MsgNativeAssimilation)
        ,("native_uprising_chance"            , numericIcon "native uprising chance" MsgNativeUprisingChance)
        ,("naval_tradition_from_battle"       , numericIcon "naval tradition from battles" MsgNavalTraditionFromBattle)
        ,("naval_tradition_from_trade"        , numericIcon "naval tradition from protecting trade" MsgNavalTraditionFromTrade)
        ,("nobles_influence_modifier"         , numericIcon "nobility influence" MsgNoblesInfluenceModifier)
        ,("nobles_loyalty_modifier"           , numericIcon "nobility loyalty" MsgNoblesLoyaltyModifier)
        ,("own_coast_naval_combat_bonus"      , numericIcon "naval combat bonus off owned coast" MsgOwnCoastNavalCombatBonus)
        ,("placed_merchant_power"             , numericIcon "merchant trade power" MsgPlacedMerchantPower)
        ,("possible_adm_policy"               , numericIcon "administrative possible policies" MsgPossibleAdmPolicy)
        ,("possible_dip_policy"               , numericIcon "diplomatic possible policies" MsgPossibleDipPolicy)
        ,("possible_mil_policy"               , numericIcon "military possible policies" MsgPossibleMilPolicy)
        ,("possible_policy"                   , numericIcon "possible policies" MsgPossiblePolicy)
        ,("power_projection_from_insults"     , numericIcon "power projection from insults" MsgPowerProjectionFromInsults)
        ,("pr_captains_influence"             , numericIcon "captains influence" MsgPrCaptainsInfluence)
        ,("province_has_center_of_trade_of_level" , numericIcon "center of trade" MsgProviceHasCenterOfTrade)
        ,("reelection_cost"                   , numericIcon "reelection cost" MsgReelectionCost)
        ,("reform_progress_growth"            , numericIcon "reform progress growth" MsgReformProgressGrowth)
        ,("reinforce_cost_modifier"           , numericIcon "reinforce cost" MsgReinforceCostModifier)
        ,("rival_border_fort_maintenance"     , numericIcon "fort maintenance on border with rival" MsgRivalBorderFortMaintenance)
        ,("sailor_maintenance_modifer"        , numericIcon "sailor maintenance" MsgSailorMaintenanceModifer)
        ,("sailors_recovery_speed"            , numericIcon "sailor recovery speed" MsgSailorsRecoverySpeed)
        ,("same_culture_advisor_cost"         , numericIcon "cost of advisors with ruler's culture" MsgSameCultureAdvisorCost)
        ,("shock_damage_received"             , numericIcon "shock damage received" MsgShockDamageReceived)
        ,("siege_blockade_progress"           , numericIcon "blockade impact on siege" MsgSiegeBlockadeProgress)
        ,("special_unit_forcelimit"           , numericIcon "special unit force limit" MsgSpecialUnitForcelimit)
        ,("sunk_ship_morale_hit_recieved"     , numericIcon "morale hit when losing a ship" MsgSunkShipMoraleHitRecieved)
        ,("trade_company_investment_cost"     , numericIcon "trade company investment cost" MsgTradeCompanyInvestmentCost)
        ,("transport_cost"                    , numericIcon "transport cost" MsgTransportCost)
        ,("treasure_fleet_income"             , numericIcon "treasure fleet income" MsgTreasureFleetIncome)
        ,("vaisyas_loyalty_modifier"          , numericIcon "vaishyas loyalty" MsgVaisyasLoyaltyModifier)
        ,("warscore_cost_vs_other_religion"   , numericIcon "war score cost vs other religions" MsgWarscoreCostVsOtherReligion)
        ,("yearly_harmony"                    , numericIcon "yearly harmony increase" MsgYearlyHarmony)
        ,("yearly_revolutionary_zeal"         , numericIcon "revolutionary zeal" MsgYearlyRevolutionaryZeal)
        ]

-- | Handlers for statements pertaining to modifiers
handlersModifiers :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersModifiers = Tr.fromList
        [("add_country_modifier"           , addModifier MsgCountryMod)
        ,("add_disaster_modifier"          , addModifier MsgDisasterMod)
        ,("add_permanent_province_modifier", addModifier MsgPermanentProvMod)
        ,("add_province_modifier"          , addModifier MsgProvMod)
        ,("add_ruler_modifier"             , addModifier MsgRulerMod)
        ,("add_trade_modifier"             , addModifier MsgTradeMod)
        ,("has_country_modifier"           , withLocAtom2 MsgCountryMod MsgHasModifier)
        ,("has_province_modifier"          , withLocAtom2 MsgProvMod MsgHasModifier)
        ,("has_ruler_modifier"             , withLocAtom2 MsgRulerMod MsgHasModifier)
        ,("has_trade_modifier"             , tradeMod)
        ,("remove_country_modifier"        , withLocAtom2 MsgCountryMod MsgRemoveModifier)
        ,("remove_province_modifier"       , withLocAtom2 MsgProvMod MsgRemoveModifier)
        ]

-- | Handlers for simple compound statements
handlersCompound :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersCompound = Tr.fromList
        -- Note that "any" can mean "all" or "one or more" depending on context.
        [("and" , compoundMessage MsgAllOf)
        ,("root", compoundMessagePronoun)
        ,("from", compoundMessagePronoun)
        ,("prev", compoundMessagePronoun)
        -- no THIS, not used on LHS
        ,("not" , compoundMessage MsgNoneOf)
        ,("or"  , compoundMessage MsgAtLeastOneOf)
        -- Tagged blocks
        ,("event_target", compoundMessageTagged MsgEventTarget (Just EU4From))
        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>."
        ,("all_core_province"       , scope EU4Province  . compoundMessage MsgAllCoreProvince)
        ,("all_country" {- sic -}   , scope EU4Country   . compoundMessage MsgAllCountries)
        ,("all_neighbor_country"    , scope EU4Country   . compoundMessage MsgAllNeighborCountries)
        ,("all_owned_province"      , scope EU4Province  . compoundMessage MsgEveryOwnedProvince)
        ,("all_province"            , scope EU4Province  . compoundMessage MsgAllProvince)
        ,("all_subject_country"     , scope EU4Country   . compoundMessage MsgAllSubjectCountries)
        ,("any_active_trade_node"   , scope EU4TradeNode . compoundMessage MsgAnyActiveTradeNode)
        ,("any_ally"                , scope EU4Country   . compoundMessage MsgAnyAlly)
        ,("any_core_country"        , scope EU4Country   . compoundMessage MsgAnyCoreCountry) -- used in province scope
        ,("any_core_province"       , scope EU4Province  . compoundMessage MsgAnyCoreProvince)
        ,("any_country"             , scope EU4Country   . compoundMessage MsgAnyCountry)
        ,("any_empty_neighbor_province", scope EU4Province  . compoundMessage MsgAnyEmptyNeighborProvince)
        ,("any_enemy_country"       , scope EU4Country   . compoundMessage MsgAnyEnemyCountry)
        ,("any_heretic_province"    , scope EU4Province  . compoundMessage MsgAnyHereticProvince)
        ,("any_known_country"       , scope EU4Country   . compoundMessage MsgAnyKnownCountry)
        ,("any_neighbor_country"    , scope EU4Country   . compoundMessage MsgAnyNeighborCountry)
        ,("any_neighbor_province"   , scope EU4Province  . compoundMessage MsgAnyNeighborProvince)
        ,("any_owned_province"      , scope EU4Province  . compoundMessage MsgAnyOwnedProvince)
        ,("any_privateering_country", scope EU4TradeNode . compoundMessage MsgAnyPrivateeringCountry)
        ,("any_province"            , scope EU4Province  . compoundMessage MsgAnyProvince)
        ,("any_rival_country"       , scope EU4Country   . compoundMessage MsgAnyRival)
        ,("any_subject_country"     , scope EU4Country   . compoundMessage MsgAnySubject)
        ,("any_trade_node"          , scope EU4TradeNode . compoundMessage MsgAnyTradeNode)
        ,("any_trade_node_member_province" , scope EU4Province . compoundMessage MsgAnyTradeNodeProvince)
        ,("capital_scope"           , scope EU4Province  . compoundMessage MsgCapital)
        ,("colonial_parent"         , scope EU4Country   . compoundMessage MsgColonialParent)
        ,("controller"              , scope EU4Country   . compoundMessage MsgController)
        ,("else"                    ,                      compoundMessage MsgElse)
        ,("else_if"                 ,                      compoundMessage MsgElseIf)
        ,("emperor"                 , scope EU4Country   . compoundMessage MsgEmperor)
        ,("every_active_trade_node" , scope EU4TradeNode . compoundMessage MsgEveryActiveTradeNode)
        ,("every_ally"              , scope EU4TradeNode . compoundMessage MsgEveryAlly)
        ,("every_core_country"      , scope EU4Country   . compoundMessage MsgEveryCoreCountry) -- used in province scope
        ,("every_core_province"     , scope EU4Province  . compoundMessage MsgEveryCoreProvince)
        ,("every_country"           , scope EU4Country   . compoundMessage MsgEveryCountry)
        ,("every_enemy_country"     , scope EU4Country   . compoundMessage MsgEveryEnemyCountry)
        ,("every_heretic_province"  , scope EU4Province  . compoundMessage MsgEveryHereticProvince)
        ,("every_known_country"     , scope EU4Country   . compoundMessage MsgEveryKnownCountry)
        ,("every_neighbor_country"  , scope EU4Country   . compoundMessage MsgEveryNeighborCountry)
        ,("every_neighbor_province" , scope EU4Province  . compoundMessage MsgEveryNeighborProvince)
        ,("every_owned_province"    , scope EU4Province  . compoundMessage MsgEveryOwnedProvince)
        ,("every_province"          , scope EU4Province  . compoundMessage MsgEveryProvince)
        ,("every_rival_country"     , scope EU4Country   . compoundMessage MsgEveryRival)
        ,("every_subject_country"   , scope EU4Country   . compoundMessage MsgEverySubject)
        ,("hidden_effect"           ,                      compoundMessage MsgHiddenEffect)
        ,("if"                      ,                      compoundMessage MsgIf) -- always needs editing
        ,("limit"                   ,                      compoundMessage MsgLimit) -- always needs editing
        ,("most_province_trade_power", scope EU4Country  . compoundMessage MsgMostProvinceTradePower)
        ,("overlord"                , scope EU4Country   . compoundMessage MsgOverlord)
        ,("owner"                   , scope EU4Country   . compoundMessage MsgOwner)
        ,("random_active_trade_node", scope EU4TradeNode . compoundMessage MsgRandomActiveTradeNode)
        ,("random_ally"             , scope EU4Country   . compoundMessage MsgRandomAlly)
        ,("random_core_country"     , scope EU4Country   . compoundMessage MsgRandomCoreCountry)
        ,("random_core_province"    , scope EU4Country   . compoundMessage MsgRandomCoreProvince)
        ,("random_country"          , scope EU4Country   . compoundMessage MsgRandomCountry)
        ,("random_elector"          , scope EU4Country   . compoundMessage MsgRandomElector)
        ,("random_enemy_country"    , scope EU4Country   . compoundMessage MsgRandomEnemyCountry)
        ,("random_empty_neighbor_province", scope EU4Province . compoundMessage MsgRandomEmptyNeighborProvince)
        ,("random_heretic_province"    , scope EU4Province  . compoundMessage MsgRandomHereticProvince)
        ,("random_known_country"    , scope EU4Country   . compoundMessage MsgRandomKnownCountry)
        ,("random_neighbor_country" , scope EU4Country   . compoundMessage MsgRandomNeighborCountry)
        ,("random_neighbor_province", scope EU4Province  . compoundMessage MsgRandomNeighborProvince)
        ,("random_owned_province"   , scope EU4Province  . compoundMessage MsgRandomOwnedProvince)
        ,("random_privateering_country", scope EU4TradeNode . compoundMessage MsgRandomPrivateeringCountry)
        ,("random_province"         , scope EU4Province  . compoundMessage MsgRandomProvince)
        ,("random_rival_country"    , scope EU4Country   . compoundMessage MsgRandomRival)
        ,("random_subject_country"  , scope EU4Country   . compoundMessage MsgRandomSubjectCountry)
        ,("random_trade_node"       , scope EU4TradeNode . compoundMessage MsgRandomTradeNode)
        ,("region_for_scope_province" , scope EU4Province . compoundMessage MsgRegionProvinceScope)
        ,("strongest_trade_power"   , scope EU4Country   . compoundMessage MsgStrongestTradePower) -- always needs editing
        ,("trigger"                 ,                      compoundMessage MsgTrigger) -- observed in random_list (probably something unhandled if seen elsewhere)
        ,("while"                   , scope EU4Country   . compoundMessage MsgWhile) -- always needs editing
        ]

-- | Handlers for simple statements where RHS is a localizable atom
handlersLocRhs :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersLocRhs = Tr.fromList
        [("add_great_project"     , withLocAtom MsgStartConstructingGreatProject)
        ,("add_government_reform" , withLocAtom MsgAddGovernmentReform)
        ,("change_government"     , withLocAtom MsgChangeGovernment)
        ,("change_province_name"  , withLocAtom MsgChangeProvinceName) -- will usually fail localization
        ,("colonial_region"       , withLocAtom MsgColonialRegion)
        ,("current_debate"        , withLocAtom MsgCurrentDebate)
        ,("end_disaster"          , withLocAtom MsgDisasterEnds)
        ,("government"            , withLocAtom MsgGovernmentIs)
        ,("has_advisor"           , withLocAtom MsgHasAdvisor)
        ,("has_active_policy"     , withLocAtom MsgHasActivePolicy)
        ,("has_construction"      , withLocAtom MsgConstructing)
        ,("has_disaster"          , withLocAtom MsgDisasterOngoing)
        ,("has_great_project"     , withLocAtom MsgConstructingGreatProject)
        ,("has_idea"              , withLocAtom MsgHasIdea)
        ,("has_reform"            , withLocAtom MsgHasReform)
        ,("has_terrain"           , withLocAtom MsgHasTerrain)
        ,("is_subject_of_type"    , withLocAtom' MsgIsSubjectOfType (\t -> t <> "_title"))
        ,("kill_advisor"          , withLocAtom MsgAdvisorDies)
        ,("region"                , withLocAtom MsgRegionIs)
        ,("remove_advisor"        , withLocAtom MsgLoseAdvisor)
        ,("rename_capital"        , withLocAtom MsgRenameCapital) -- will usually fail localization
        ,("superregion"           , withLocAtom MsgSuperRegionIs)
        ]

-- | Handlers for statements whose RHS is a province ID
handlersProvince :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersProvince = Tr.fromList
        [("capital"           , withProvince MsgCapitalIs)
        ,("controls"          , withProvince MsgControls)
        ,("discover_province" , withProvince MsgDiscoverProvince)
        ,("owns"              , withProvince MsgOwns)
        ,("owns_core_province", withProvince MsgOwnsCore)
        ,("owns_or_vassal_of" , withProvince MsgOwnsOrVassal)
        ,("province_id"       , withProvince MsgProvinceIs)
        ,("set_capital"       , withProvince MsgSetCapital)
        ]

-- | Handlers for statements whose RHS is a flag OR a province ID
-- Also abusable for tag,scope purposes
handlersFlagOrProvince :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrProvince = Tr.fromList
        [("add_claim"          , withFlagOrProvince MsgAddClaimFor MsgAddClaimOn)
        ,("add_permanent_claim", withFlagOrProvince MsgGainPermanentClaimCountry MsgGainPermanentClaimProvince)
        ,("cavalry"            , withFlagOrProvince MsgCavalrySpawnsCountry MsgCavalrySpawnsProvince)
        ,("infantry"           , withFlagOrProvince MsgInfantrySpawnsCountry MsgInfantrySpawnsProvince)
        ,("remove_core"        , withFlagOrProvince MsgLoseCoreCountry MsgLoseCoreProvince)
        ,("is_colonial_nation_of" , withFlagOrProvince MsgIsColonialNationOf MsgIsColonialNationOf)
        -- RHS is a flag or province id, but the statement's meaning depends on the scope
        ,("has_discovered"     , withFlagOrProvinceEU4Scope MsgHasDiscovered MsgHasDiscovered MsgDiscoveredBy MsgDiscoveredBy) -- scope sensitive
        ,("same_continent"     , withFlagOrProvinceEU4Scope (MsgSameContinent True True) (MsgSameContinent True False) (MsgSameContinent False True) (MsgSameContinent False False)) -- scope sensitive
        ]

-- | Handlers for statements whose RHS is a number OR a tag/pronoun, with icon
handlersNumericOrFlag :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericOrFlag = Tr.fromList
        [("adm_tech", withTagOrNumber "adm tech" MsgADMTech MsgADMTechAs)
        ]

-- TODO: parse advisor files
-- | Handlers for statements whose RHS is an advisor ID
handlersAdvisorId :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersAdvisorId = Tr.fromList
        [("advisor_exists"     , numeric MsgAdvisorExists)
        ,("is_advisor_employed", numeric MsgAdvisorIsEmployed)
        ]

-- | Simple statements whose RHS should be presented as is, in typewriter face
handlersTypewriter :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersTypewriter = Tr.fromList
        [("clr_consort_flag" , withNonlocAtom2 MsgConsortFlag MsgClearFlag)
        ,("clr_country_flag" , withNonlocAtom2 MsgCountryFlag MsgClearFlag)
        ,("clr_province_flag", withNonlocAtom2 MsgProvinceFlag MsgClearFlag)
        ,("clr_ruler_flag"   , withNonlocAtom2 MsgRulerFlag MsgClearFlag)
        ,("clear_saved_name" , withNonlocAtom MsgClearSavedName)
        ,("has_consort_flag" , withNonlocAtom2 MsgConsortFlag MsgHasFlag)
        ,("has_country_flag" , withNonlocAtom2 MsgCountryFlag MsgHasFlag)
        ,("has_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgHasFlag)
        ,("has_province_flag", withNonlocAtom2 MsgProvinceFlag MsgHasFlag)
        ,("has_ruler"        , withNonlocAtom MsgHasRuler)
        ,("has_ruler_flag"   , withNonlocAtom2 MsgRulerFlag MsgHasFlag)
        ,("has_saved_event_target", withNonlocAtom MsgHasSavedEventTarget)
        ,("save_event_target_as", withNonlocAtom MsgSaveEventTargetAs)
        ,("set_consort_flag" , withNonlocAtom2 MsgConsortFlag MsgSetFlag)
        ,("set_country_flag" , withNonlocAtom2 MsgCountryFlag MsgSetFlag)
        ,("set_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgSetFlag)
        ,("set_province_flag", withNonlocAtom2 MsgProvinceFlag MsgSetFlag)
        ,("set_ruler_flag"   , withNonlocAtom2 MsgRulerFlag MsgSetFlag)
        ]

-- | Handlers for simple statements with icon
handlersSimpleIcon :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleIcon = Tr.fromList
        [("accepted_culture"        , withLocAtomAndIcon "max promoted cultures" MsgAcceptedCulture)
        ,("add_accepted_culture"    , withLocAtomAndIcon "max promoted cultures" MsgAddAcceptedCulture)
        ,("add_building"            , withLocAtomIconBuilding MsgAddBuilding)
        ,("add_harmonized_religion" , withLocAtomIcon MsgAddHarmonizedReligion)
        ,("add_heir_personality"    , withLocAtomIcon MsgAddHeirPersonality)
        ,("add_queen_personality"   , withLocAtomIcon MsgAddConsortPersonality)
        ,("add_reform_center"       , withLocAtomIcon MsgAddCenterOfReformation)
        ,("add_ruler_personality"   , withLocAtomIcon MsgAddRulerPersonality)
        ,("remove_ruler_personality"  , withLocAtomIcon MsgRemoveRulerPersonality)
        ,("advisor"                 , withLocAtomIcon MsgHasAdvisorType)
        ,("change_technology_group" , withLocAtomIcon MsgChangeTechGroup)
        ,("change_trade_goods"      , withLocAtomIcon MsgChangeGoods)
        ,("change_unit_type"        , withLocAtomIcon MsgChangeUnitType)
        ,("consort_has_personality" , withLocAtomIcon MsgConsortHasPersonality)
        ,("create_advisor"          , withLocAtomIcon MsgCreateAdvisor)
        ,("current_age"             , withLocAtomIcon MsgCurrentAge)
        ,("enable_religion"         , withLocAtomIcon MsgEnableReligion)
        ,("has_adopted_cult"        , withLocAtomIcon MsgHasAdoptedCult)
        ,("has_building"            , withLocAtomIconBuilding MsgHasBuilding)
        ,("has_idea_group"          , withLocAtomIcon MsgHasIdeaGroup) -- FIXME: icon fails
        ,("has_institution"         , withLocAtomIcon MsgHasInstitution)
        ,("heir_has_personality"    , withLocAtomIcon MsgHeirHasPersonality)
        ,("ruler_has_personality"   , withLocAtomIcon MsgRulerHasPersonality)
        ,("has_unlocked_cult"       , withLocAtomIcon MsgHasUnlockedCult)
        ,("full_idea_group"         , withLocAtomIcon MsgFullIdeaGroup)
        ,("hre_religion"            , withLocAtomIcon MsgHREReligion)
        ,("is_religion_enabled"     , withLocAtomIcon MsgReligionEnabled)
        ,("remove_estate"           , withLocAtomIcon MsgRemoveFromEstate )
        ,("secondary_religion"      , withLocAtomIcon MsgSecondaryReligion)
        ,("set_hre_heretic_religion", withLocAtomIcon MsgSetHREHereticReligion)
        ,("set_hre_religion"        , withLocAtomIcon MsgSetHREReligion)
        ,("technology_group"        , withLocAtomIcon MsgTechGroup)
        ,("unlock_cult"             , withLocAtomIcon MsgUnlockCult)
        ,("has_estate"              , withLocAtomIconEU4Scope MsgEstateExists MsgHasEstate)
        ,("set_estate"              , withLocAtomIcon MsgAssignToEstate)
        ,("is_monarch_leader"       , withLocAtomAndIcon "ruler general" MsgRulerIsGeneral)
        ]

-- | Handlers for simple statements with a flag
handlersSimpleFlag :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersSimpleFlag = Tr.fromList
        [("add_claim"               , withFlag MsgGainClaim)
        ,("add_historical_rival"    , withFlag MsgAddHistoricalRival)
        ,("add_truce_with"          , withFlag MsgAddTruceWith)
        ,("alliance_with"           , withFlag MsgAlliedWith)
        ,("cede_province"           , withFlag MsgCedeProvinceTo)
        ,("change_tag"              , withFlag MsgChangeTag)
        ,("controlled_by"           , withFlag MsgControlledBy)
        ,("country_or_non_sovereign_subject_holds" , withFlag MsgCountryOrNonSovereignSubjectHolds)
        ,("create_alliance"         , withFlag MsgCreateAlliance)
        ,("defensive_war_with"      , withFlag MsgDefensiveWarAgainst)
        ,("discover_country"        , withFlag MsgDiscoverCountry)
        ,("free_vassal"             , withFlag MsgFreeVassal)
        ,("galley"                  , withFlag MsgGalley)
        ,("has_merchant"            , withFlag MsgHasMerchant)
        ,("heavy_ship"              , withFlag MsgHeavyShip)
        ,("inherit"                 , withFlag MsgInherit)
        ,("is_enemy"                , scope EU4Country . withFlag MsgIsEnemy)
        ,("is_league_enemy"         , withFlag MsgIsLeagueEnemy)
        ,("is_neighbor_of"          , withFlag MsgNeighbors)
        ,("is_permanent_claim"      , withFlag MsgIsPermanentClaim)
        ,("is_rival"                , withFlag MsgIsRival)
        ,("is_strongest_trade_power", withFlag MsgIsStrongestTradePower)
        ,("is_subject_of"           , withFlag MsgIsSubjectOf)
        ,("junior_union_with"       , withFlag MsgJuniorUnionWith)
        ,("light_ship"              , withFlag MsgLightShip)
        ,("marriage_with"           , withFlag MsgRoyalMarriageWith)
        ,("offensive_war_with"      , withFlag MsgOffensiveWarAgainst)
        ,("overlord_of"             , withFlag MsgOverlordOf)
        ,("owned_by"                , withFlag MsgOwnedBy)
        ,("release"                 , withFlag MsgReleaseVassal)
        ,("remove_claim"            , withFlag MsgRemoveClaim)
        ,("senior_union_with"       , withFlag MsgSeniorUnionWith)
        ,("sieged_by"               , withFlag MsgUnderSiegeBy)
        ,("support_independence_of" , withFlag MsgSupportIndependenceOf)
        ,("tag"                     , withFlag MsgCountryIs)
        ,("truce_with"              , withFlag MsgTruceWith)
        ,("vassal_of"               , withFlag MsgVassalOf)
        ,("war_with"                , withFlag MsgAtWarWith)
        ,("white_peace"             , withFlag MsgMakeWhitePeace)
        ]

-- | Handlers for simple generic statements with a flag or "yes"/"no"
handlersFlagOrYesNo :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrYesNo = Tr.fromList
        [("exists", withFlagOrBool MsgExists MsgCountryExists)
        ]

-- | Handlers for statements whose RHS may be an icon, a flag, a province, or a
-- pronoun (such as ROOT).
handlersIconFlagOrPronoun :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersIconFlagOrPronoun = Tr.fromList
        [("change_culture"   , locAtomTagOrProvince (const MsgChangeCulture) MsgChangeSameCulture)
        -- above is province, below is country - use same messages for both
        ,("change_primary_culture", locAtomTagOrProvince (const MsgChangeCulture) MsgChangeSameCulture)
        ,("change_religion"  , iconOrFlag MsgChangeReligion MsgChangeSameReligion Nothing)
        ,("continent"        , locAtomTagOrProvince (const MsgContinentIs) MsgContinentIsAs)
        ,("culture"          , locAtomTagOrProvince (const MsgCultureIs) MsgCultureIsAs)
        ,("culture_group"    , locAtomTagOrProvince (const MsgCultureIsGroup) MsgCultureGroupAs)
        ,("dominant_religion", locAtomTagOrProvince MsgDominantReligion MsgDominantReligionAs)
        ,("heir_nationality" , locAtomTagOrProvince (const MsgHeirNationality) MsgHeirNationalityAs)
        ,("heir_religion"    , locAtomTagOrProvince MsgHeirReligion MsgHeirReligionAs)
        ,("is_core"          , tagOrProvince MsgIsCoreOf MsgHasCoreOn (Just EU4Country))
        ,("is_claim"         , tagOrProvince MsgHasClaim MsgHasClaimOn (Just EU4Country))
        ,("primary_culture"  , locAtomTagOrProvince (const MsgPrimaryCultureIs) MsgPrimaryCultureIsAs)
        ,("religion"         , locAtomTagOrProvince MsgReligion MsgSameReligion)
        ,("religion_group"   , locAtomTagOrProvince MsgReligionGroup MsgSameReligionGroup)
        ,("set_heir_religion", locAtomTagOrProvince MsgSetHeirReligion MsgSetHeirReligionAs)
        ,("trade_goods"      , locAtomTagOrProvince MsgProducesGoods MsgProducesSameGoods)
        ]

-- | Handlers for statements whose RHS may be either a tag or a province
handlersTagOrProvince :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersTagOrProvince = Tr.fromList
        [ -- obsolete
        ]

-- | Handlers for yes/no statements
handlersYesNo :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersYesNo = Tr.fromList
        [("ai"                          , withBool MsgIsAIControlled)
        ,("allows_female_emperor"       , withBool MsgFemaleEmperorAllowed)
        ,("always"                      , withBool MsgAlways)
        ,("back_current_issue"          , withBool MsgBackCurrentIssue)
        ,("has_active_debate"           , withBool MsgHasActiveDebate)
        ,("has_any_disaster"            , withBool MsgHasAnyDisaster)
        ,("has_cardinal"                , withBool MsgHasCardinal)
        ,("has_completed_all_reforms_trigger" , withBool MsgHasCompletedAllReforms)
        ,("has_consort"                 , withBool MsgHasConsort)
        ,("has_custom_ideas"            , withBool MsgHasCustomIdeas)
        ,("has_factions"                , withBool MsgHasFactions)
        ,("has_female_heir"             , withBool MsgHasFemaleHeir)
        ,("has_foreign_heir"            , withBool MsgHasForeignHeir)
        ,("has_heir"                    , withBool MsgHasHeir)
        ,("has_missionary"              , withBool MsgHasMissionary)
        ,("has_owner_accepted_culture"  , withBool MsgHasOwnerAcceptedCulture)
        ,("has_owner_culture"           , withBool MsgHasOwnerCulture)
        ,("has_owner_religion"          , withBool MsgHasOwnerReligion)
        ,("has_parliament"              , withBool MsgHasParliament)
        ,("has_port"                    , withBool MsgHasPort)
        ,("has_revolution_in_province"  , withBool MsgHasRevolutionInProvince)
        ,("has_seat_in_parliament"      , withBool MsgHasSeatInParliament)
        ,("has_states_general_mechanic" , withBool MsgHasStatesGeneralMechanic)
        ,("has_regency"                 , withBool MsgIsInRegency)
        ,("has_religious_school"        , withBool MsgHasReligiousSchool)
        ,("has_siege"                   , withBool MsgUnderSiege)
        ,("has_secondary_religion"      , withBool MsgHasSecondaryReligion)
        ,("has_truce"                   , withBool MsgHasTruce)
        ,("has_wartaxes"                , withBool MsgHasWarTaxes)
        ,("hre_leagues_enabled"         , withBool MsgHRELeaguesEnabled)
        ,("hre_religion_locked"         , withBool MsgHREReligionLocked)
        ,("hre_religion_treaty"         , withBool MsgHREWestphalia)
        ,("is_at_war"                   , withBool MsgAtWar)
        ,("is_bankrupt"                 , withBool MsgIsBankrupt)
        ,("is_backing_current_issue"    , withBool MsgIsBackingCurrentIssue)
        ,("is_capital"                  , withBool MsgIsCapital)
        ,("is_center_of_revolution"     , withBool MsgIsCenterOfRevolution)
        ,("is_city"                     , withBool MsgIsCity)
        ,("is_colony"                   , withBool MsgIsColony)
        ,("is_colonial_nation"          , withBool MsgIsColonialNation)
        ,("is_defender_of_faith"        , withBool MsgIsDefenderOfFaith)
        ,("is_emperor_of_china"         , withBool MsgIsEmperorOfChina)
        ,("is_empty"                    , withBool MsgIsProvinceEmpty)
        ,("is_force_converted"          , withBool MsgWasForceConverted)
        ,("is_former_colonial_nation"   , withBool MsgIsFormerColonialNation)
        ,("is_free_or_tributary_trigger", withBool MsgIsFreeOrTributaryTrigger)
        ,("is_elector"                  , withBool MsgIsElector)
        ,("is_emperor"                  , withBool MsgIsEmperor)
        ,("is_female"                   , withBool MsgIsFemale)
        ,("is_great_power"              , withBool MsgIsGreatPower)
        ,("is_in_capital_area"          , withBool MsgIsInCapitalArea)
        ,("is_in_league_war"            , withBool MsgIsInLeagueWar)
        ,("is_lesser_in_union"          , withBool MsgIsLesserInUnion)
        ,("is_looted"                   , withBool MsgIsLooted)
        ,("is_nomad"                    , withBool MsgIsNomad)
        ,("is_orangists_in_power"       , withBool MsgIsOrangistsInPower)
        ,("is_overseas"                 , withBool MsgIsOverseas)
        ,("is_part_of_hre"              , withBool MsgIsPartOfHRE)
        ,("is_playing_custom_nation"    , withBool MsgIsCustomNation)
        ,("is_random_new_world"         , withBool MsgRandomNewWorld)
        ,("is_reformation_center"       , withBool MsgIsCenterOfReformation)
        ,("is_religion_reformed"        , withBool MsgReligionReformed)
        ,("is_revolutionary"            , withBool MsgIsRevolutionary)
        ,("is_revolutionary_republic_trigger" , withBool MsgIsRevolutionaryRepublic)
        ,("is_sea"                      , withBool MsgIsSea) -- province or trade node
        ,("is_state"                    , withBool MsgIsState)
        ,("is_statists_in_power"        , withBool MsgIsStatistsInPower)
        ,("is_subject"                  , withBool MsgIsSubject)
        ,("is_tribal"                   , withBool MsgIsTribal)
        ,("is_tutorial_active"          , withBool MsgIsInTutorial)
        ,("luck"                        , withBool MsgLucky)
        ,("normal_or_historical_nations", withBool MsgNormalOrHistoricalNations)
        ,("papacy_active"               , withBool MsgPapacyIsActive)
        ,("primitives"                  , withBool MsgPrimitives)
        ,("revolution_target_exists"    , withBool MsgRevolutionTargetExists)
        ,("ruler_is_foreigner"          , withBool MsgRulerIsForeigner)
        ,("set_hre_religion_locked"     , withBool MsgSetHREReligionLocked)
        ,("set_in_empire"               , withBool MsgSetInEmpire)
        ,("set_seat_in_parliament"      , withBool MsgSetSeatInParliament)
        ,("set_revolution_in_province"  , withBool MsgSetRevolutionProvince)
        ,("unit_in_siege"               , withBool MsgUnderSiege) -- duplicate?
        ,("valid_for_personal_unions_trigger" , withBool MsgValidForPU)
        ,("was_player"                  , withBool MsgHasBeenPlayer)
        ,("was_never_end_game_tag_trigger" , withBool MsgWasNeverEndGameTag)
        ]

-- | Handlers for statements that may be numeric or a tag
handlersNumericOrTag :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericOrTag = Tr.fromList
        [("num_of_cities"       , numericOrTag MsgNumCities MsgNumCitiesThan)
        ,("army_professionalism", numericOrTagIcon "army professionalism" MsgArmyProfessionalism MsgArmyProfessionalismAs)
        ]

-- | Handlers for signed numeric statements
handlersSignedNumeric :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersSignedNumeric = Tr.fromList
        [("tolerance_to_this", numeric MsgToleranceToThis)
        ]

-- | Handlers querying the number of provinces of some kind, mostly religions
-- and trade goods
handlersNumProvinces :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumProvinces = Tr.fromList
        [("animism"       , numProvinces "animism" MsgReligionProvinces)
        ,("catholic"      , numProvinces "catholic" MsgReligionProvinces)
        ,("chinaware"     , numProvinces "chinaware" MsgGoodsProvinces)
        ,("cocoa"         , numProvinces "cloth" MsgGoodsProvinces)
        ,("coffee"        , numProvinces "cloth" MsgGoodsProvinces)
        ,("confucianism"  , numProvinces "cloth" MsgReligionProvinces)
        ,("coptic"        , numProvinces "cloth" MsgReligionProvinces)
        ,("cloth"         , numProvinces "cloth" MsgGoodsProvinces)
        ,("copper"        , numProvinces "copper" MsgGoodsProvinces)
        ,("cotton"        , numProvinces "copper" MsgGoodsProvinces)
        ,("fish"          , numProvinces "fish" MsgGoodsProvinces)
        ,("fur"           , numProvinces "fur" MsgGoodsProvinces)
        ,("gold"          , numProvinces "gold" MsgGoodsProvinces)
        ,("grain"         , numProvinces "grain" MsgGoodsProvinces)
        ,("hinduism"      , numProvinces "hinduism" MsgReligionProvinces)
        ,("ibadi"         , numProvinces "iron" MsgReligionProvinces)
        ,("iron"          , numProvinces "iron" MsgGoodsProvinces)
        ,("ivory"         , numProvinces "ivory" MsgGoodsProvinces)
        ,("livestock"     , numProvinces "fish" MsgGoodsProvinces)
        ,("mahayana"      , numProvinces "fish" MsgReligionProvinces)
        ,("naval_supplies", numProvinces "fish" MsgGoodsProvinces)
        ,("orthodox"      , numProvinces "orthodox" MsgReligionProvinces)
        ,("protestant"    , numProvinces "orthodox" MsgReligionProvinces)
        ,("reformed"      , numProvinces "orthodox" MsgReligionProvinces)
        ,("salt"          , numProvinces "salt" MsgGoodsProvinces)
        ,("shiite"        , numProvinces "salt" MsgReligionProvinces)
        ,("shinto"        , numProvinces "salt" MsgReligionProvinces)
        ,("sikhism"       , numProvinces "salt" MsgReligionProvinces)
        ,("slaves"        , numProvinces "slaves" MsgGoodsProvinces)
        ,("spices"        , numProvinces "spices" MsgGoodsProvinces)
        ,("sugar"         , numProvinces "spices" MsgGoodsProvinces)
        ,("sunni"         , numProvinces "spices" MsgReligionProvinces)
        ,("tea"           , numProvinces "spices" MsgGoodsProvinces)
        ,("tobacco"       , numProvinces "spices" MsgGoodsProvinces)
        ,("totemism"      , numProvinces "spices" MsgReligionProvinces)
        ,("wine"          , numProvinces "wine" MsgGoodsProvinces)
        ,("wool"          , numProvinces "wool" MsgGoodsProvinces)
        ]

-- | Handlers for text/value pairs.
--
-- $textvalue
handlersTextValue :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextValue = Tr.fromList
        [("add_incident_variable_value" , textValue "incident" "value" MsgAddIncidentVariableValue MsgAddIncidentVariableValue tryLocAndIcon)
        ,("add_institution_embracement" , textValue "which" "value" MsgAddInstitutionEmbracement MsgAddInstitutionEmbracement tryLocAndIcon)
        ,("add_estate_loyalty"          , textValue "estate" "loyalty" MsgAddEstateLoyalty MsgAddEstateLoyalty tryLocAndIcon)
        ,("add_spy_network_from"        , textValue "who" "value" MsgAddSpyNetworkFrom MsgAddEstateLoyalty flagTextMaybe)
        ,("add_spy_network_in"          , textValue "who" "value" MsgAddSpyNetworkIn MsgAddEstateLoyalty flagTextMaybe )
        ,("check_variable"              , textValue "which" "value" MsgCheckVariable MsgCheckVariable tryLocAndIcon)
        ,("estate_influence"            , textValue "estate" "influence" MsgEstateInfluence MsgEstateInfluence tryLocAndIcon)
        ,("estate_influence"            , textValue "estate" "influence" MsgEstateInfluence MsgEstateInfluence tryLocAndIcon)
        ,("estate_loyalty"              , textValue "estate" "loyalty" MsgEstateLoyalty MsgEstateLoyalty tryLocAndIcon)
        ,("estate_loyalty"              , textValue "estate" "loyalty" MsgEstateLoyalty MsgEstateLoyalty tryLocAndIcon)
        ,("estate_territory"            , textValue "estate" "territory" MsgEstateTerritory MsgEstateTerritory tryLocAndIcon)
        ,("had_country_flag"            , textValue "flag" "days" MsgHadCountryFlag MsgHadCountryFlag tryLocAndIcon)
        ,("had_country_flag"            , textValue "flag" "days" MsgHadCountryFlag MsgHadCountryFlag tryLocAndIcon)
        ,("had_global_flag"             , textValue "flag" "days" MsgHadGlobalFlag MsgHadGlobalFlag tryLocAndIcon)
        ,("had_global_flag"             , textValue "flag" "days" MsgHadGlobalFlag MsgHadGlobalFlag tryLocAndIcon)
        ,("had_province_flag"           , textValue "flag" "days" MsgHadProvinceFlag MsgHadProvinceFlag tryLocAndIcon)
        ,("had_province_flag"           , textValue "flag" "days" MsgHadProvinceFlag MsgHadProvinceFlag tryLocAndIcon)
        ,("had_ruler_flag"              , textValue "flag" "days" MsgHadRulerFlag MsgHadRulerFlag tryLocAndIcon)
        ,("had_ruler_flag"              , textValue "flag" "days" MsgHadRulerFlag MsgHadRulerFlag tryLocAndIcon)
        ,("has_spy_network_from"        , textValue "who" "value" MsgHasSpyNetworkFrom MsgHasSpyNetworkFrom flagTextMaybe)
        ,("num_of_religion"             , textValue "religion" "value" MsgNumOfReligion MsgNumOfReligion tryLocAndIcon)
        ,("trade_share"                 , textValue "country" "share" MsgTradeShare MsgTradeShare flagTextMaybe)
        ]

-- | Handlers for text/atom pairs
handlersTextAtom :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersTextAtom = Tr.fromList
        [("religious_school", textAtom "school" "group" MsgReligiousSchool tryLoc)
        ,("set_religious_school", textAtom "school" "group" MsgSetReligiousSchool tryLoc)
        ]

-- | Handlers for special complex statements
handlersSpecialComplex :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersSpecialComplex = Tr.fromList
        [("add_casus_belli"              , addCB True)
        ,("add_faction_influence"        , factionInfluence)
        ,("add_government_power"         , governmentPower)
        ,("add_estate_influence_modifier", estateInfluenceModifier MsgEstateInfluenceModifier)
        ,("add_mutual_opinion_modifier_effect", opinion MsgMutualOpinion MsgMutualOpinionDur)
        ,("add_opinion"                  , opinion MsgAddOpinion MsgAddOpinionDur)
        ,("add_trust"                    , trust)
        ,("reverse_add_opinion"          , opinion MsgReverseAddOpinion MsgReverseAddOpinionDur)
        ,("area"                         , area)
        ,("custom_trigger_tooltip"       , customTriggerTooltip)
        ,("build_to_forcelimit"          , buildToForcelimit)
        ,("country_event"                , scope EU4Country . triggerEvent MsgCountryEvent)
        ,("declare_war_with_cb"          , declareWarWithCB)
        ,("define_advisor"               , defineAdvisor)
        ,("define_consort"               , defineConsort)
        ,("define_heir"                  , defineHeir)
        ,("define_ruler"                 , defineRuler)
        ,("define_admiral"               , defineMilitaryLeader MsgDefineAdmiral True)
        ,("define_conquistador"          , defineMilitaryLeader MsgDefineConquistador False)
        ,("define_explorer"              , defineMilitaryLeader MsgDefineExplorer True)
        ,("define_general"               , defineMilitaryLeader MsgDefineGeneral False)
        ,("employed_advisor"             , employedAdvisor)
        ,("has_casus_belli"              , hasCasusBelli)
        ,("has_estate_influence_modifier", hasEstateInfluenceModifier)
        ,("has_opinion"                  , hasOpinion MsgHasOpinion)
        ,("has_opinion_modifier"         , opinion MsgHasOpinionMod (\modid what who _years -> MsgHasOpinionMod modid what who))
        ,("is_in_war"                    , isInWar)
        ,("privateer_power"              , privateerPower)
        ,("province_event"               , scope EU4Province . triggerEvent MsgProvinceEvent)
        ,("remove_opinion"               , opinion MsgRemoveOpinionMod (\modid what who _years -> MsgRemoveOpinionMod modid what who))
        ,("reverse_has_opinion"          , hasOpinion MsgReverseHasOpinion)
        ,("religion_years"               , religionYears)
        ,("reverse_add_casus_belli"      , addCB False)
        ,("set_variable"                 , setVariable)
        ,("trigger_switch"               , triggerSwitch)
        ]

-- | Handlers for statements pertaining to rebels
handlersRebels :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersRebels = Tr.fromList
        [("can_spawn_rebels"  , canSpawnRebels)
        ,("create_revolt"     , spawnRebels Nothing)
        ,("has_spawned_rebels", hasSpawnedRebels)
        ,("likely_rebels"     , canSpawnRebels)
        ,("spawn_rebels"      , spawnRebels Nothing)
        -- Specific rebels
        ,("anti_tax_rebels"   , spawnRebels (Just "anti_tax_rebels"))
        ,("nationalist_rebels", spawnRebels (Just "nationalist_rebels"))
        ,("noble_rebels"      , spawnRebels (Just "noble_rebels"))
        ]

-- | Handlers for idea groups
handlersIdeaGroups :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersIdeaGroups = Tr.fromList
        -- Idea groups
        [("aristocracy_ideas"   , hasIdea MsgHasAristocraticIdea)
        ,("defensive_ideas"     , hasIdea MsgHasDefensiveIdea)
        ,("economic_ideas"      , hasIdea MsgHasEconomicIdea)
        ,("innovativeness_ideas", hasIdea MsgHasInnovativeIdea)
        ,("maritime_ideas"      , hasIdea MsgHasMaritimeIdea)
        ,("offensive_ideas"     , hasIdea MsgHasOffensiveIdea)
        ]

-- | Handlers for miscellaneous statements
handlersMisc :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersMisc = Tr.fromList
        [("random", random)
        ,("random_list", randomList)
        -- Special
        ,("add_core"            , addCore)
        ,("add_manpower"        , gainMen)
        ,("add_sailors"         , gainMen)
        ,("calc_true_if"        , calcTrueIf)
        ,("dominant_culture"    , dominantCulture)
        ,("dynasty"             , dynasty)
        ,("faction_in_power"    , factionInPower)
        ,("government_rank"     , govtRank)
        ,("has_dlc"             , hasDlc)
        ,("has_government_attribute" , hasGovermentAttribute)
        ,("hre_reform_level"    , hreReformLevel)
        ,("is_month"            , isMonth)
        ,("num_of_owned_provinces_with" , numOwnedProvincesWith MsgNumOwnedProvincesWith)
        ,("num_of_provinces_owned_or_owned_by_non_sovereign_subjects_with", numOwnedProvincesWith MsgNumOwnedProvincesOrNonSovereignSubjectsWith)
        ,("piety"               , piety)
        ,("range"               , range)
        ,("set_government_rank" , setGovtRank)
        ,("set_saved_name"      , setSavedName)
        ]

-- | Handlers for ignored statements
handlersIgnored :: (EU4Info g, Monad m) => Trie (StatementHandler g m)
handlersIgnored = Tr.fromList
        [("custom_tooltip", return $ return [])
        ,("goto"          , return $ return [])
        ,("tooltip"       , return $ return [])
        ,("required_personality", return $ return[]) -- From the 1.30 patch notes: "The required_personality field will now be ignored"
        ,("highlight"     , return $ return [])
        ]

-- | Extract the appropriate message(s) from a single statement. Note that this
-- may produce many lines (via 'ppMany'), since some statements are compound.
ppOne :: (EU4Info g, Monad m) => StatementHandler g m
ppOne stmt@[pdx| %lhs = %rhs |] = case lhs of
    GenericLhs label _ -> case Tr.lookup (TE.encodeUtf8 (T.toLower label)) ppHandlers of
        Just handler -> handler stmt
        -- default
        Nothing -> if isTag label
             then case rhs of
                CompoundRhs scr ->
                    withCurrentIndent $ \_ -> do -- force indent level at least 1
                        [lflag] <- plainMsg =<< (<> ":") <$> flagText (Just EU4Country) label
                        scriptMsgs <- scope EU4Country $ ppMany scr
                        return (lflag : scriptMsgs)
                _ -> preStatement stmt
             else do
                mloc <- getGameL10nIfPresent label
                case mloc of
                    -- Check for localizable atoms, e.g. regions
                    Just loc -> case rhs of
                        CompoundRhs scr -> do
                            let (mtypeStmt, rest) = extractStmt (matchLhsText "type") scr
                            [header] <- plainMsg $ (case mtypeStmt of
                                Just [pdx| %_ = all |] -> "All provinces in "
                                _ -> "") <> loc <> ":"
                            scriptMsgs <- scope EU4Country $ ppMany rest
                            return (header : scriptMsgs)
                        _ -> compound loc stmt
                    Nothing -> preStatement stmt
    AtLhs _ -> return [] -- don't know how to handle these
    IntLhs n -> do -- Treat as a province tag
        let provN = T.pack (show n)
        prov_loc <- getGameL10nDefault ("Province " <> provN) ("PROV" <> provN)
        case rhs of
            CompoundRhs scr -> do
                header <- msgToPP (MsgProvince prov_loc)
                scriptMsgs <- ppMany scr
                return (header ++ scriptMsgs)
            _ -> preStatement stmt
    CustomLhs _ -> preStatement stmt
ppOne stmt = preStatement stmt

-- | Try to extra one matching statement
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
matchLhsText _ _ = False
