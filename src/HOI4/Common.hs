{-|
Module      : HOI4.Common
Description : Message handler for Europa Hearts of Iron IV
-}
module HOI4.Common (
        pp_script
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
pp_script :: (HOI4Info g, Monad m) =>
    GenericScript -> PPT g m Doc
pp_script [] = return "(Nothing)"
pp_script script = imsg2doc =<< ppMany script

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
handlersRhsIrrelevant :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersRhsIrrelevant = Tr.fromList
        [("active_imperial_incident" , rhsAlways "any" MsgAnyActiveTradeNode)
        ,("add_cardinal"             , rhsAlwaysYes MsgAddCardinal)
        ,("add_estate_burghers_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_burghers_loyalty_effect"))
        ,("add_estate_church_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_church_loyalty_effect"))
        ,("add_estate_cossacks_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|cossacks"))
        ,("add_estate_dhimmi_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_dhimmi_loyalty_effect"))
        ,("add_estate_jains_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|jains"))
        ,("add_estate_nobles_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_nobles_loyalty_effect"))
        ,("add_estate_rajput_loyalty_effect", rhsAlwaysYes (MsgGenericTemplate "add_estate_loyalty_effect|rajputs"))
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
        ,("can_have_center_of_reformation_trigger" , const $ msgToPP MsgCanHaveCenterOfReformation)
        ,("cancel_construction"     , rhsAlwaysYes MsgCancelConstruction) -- Canals
        ,("cb_on_overseas"          , rhsAlwaysYes MsgGainOverseasCB) -- Full Expansion
        ,("cb_on_primitives"        , rhsAlwaysYes MsgGainPrimitivesCB) -- Full Exploration
        ,("cb_on_religious_enemies" , rhsAlwaysYes MsgGainReligiousCB) -- Deus Vult
        ,("change_government_to_monarchy"  , rhsAlwaysYes $ MsgChangeGovernment "monarchy")
        ,("change_government_to_republic"  , rhsAlwaysYes $ MsgChangeGovernment "republic")
        ,("change_government_to_theocracy" , rhsAlwaysYes $ MsgChangeGovernment "theocracy")
        ,("check_if_non_state_advisor_effect", const $ msgToPP MsgCheckIfNonStateAdvisorEffect) -- Ignore actual percentages
        ,("clear_previous_primary_cults" , rhsAlwaysYes $ MsgClearPreviousPrimaryCults)
        ,("clear_rebels"            , rhsAlwaysYes MsgClearRebels)
        ,("divorce_consort_effect"  , rhsAlwaysYes MsgDivorceConsortEffect)
        ,("drop_cosmetic_tag"       , rhsAlwaysYes MsgDropCosmeticTag)
        ,("enable_hre_leagues"      , rhsAlwaysYes MsgEnableHRELeagues)
        ,("erase_advisor_flags_effect", rhsAlwaysYes MsgEnableHRELeagues)
        ,("grant_independence"      , rhsAlwaysYes MsgGrantIndependence)
        ,("has_holy_order_trigger"  , rhsAlwaysYes MsgHasAnyHolyOrder)
        ,("has_border_with_religious_enemy" , rhsAlwaysYes MsgHasBorderWithReligiousEnemy)
        ,("has_river_estuary_trigger", rhsAlwaysYes MsgHasRiverEstuary)
        ,("has_religious_scholars_trigger", rhsAlwaysYes MsgHasScholar)
        ,("has_shia_school_trigger", rhsAlwaysYes MsgHasShiaSchool)
        ,("has_sunni_school_trigger", rhsAlwaysYes MsgHasSunniSchool)
        ,("highest_supply_limit_in_area" , rhsAlwaysYes MsgHighestSupplyLimitInArea)
        ,("highest_value_trade_node", rhsAlwaysYes MsgHighestValueTradeNode)
        ,("increase_heir_adm_effect", rhsAlwaysYes MsgIncreaseHeirAdmEffect)
        ,("increase_heir_dip_effect", rhsAlwaysYes MsgIncreaseHeirDipEffect)
        ,("increase_heir_mil_effect", rhsAlwaysYes MsgIncreaseHeirMilEffect)
        ,("increase_legitimacy_huge_effect"  , rhsAlwaysYes MsgIncreaseLegitimacyHugeEffect)
        ,("increase_legitimacy_medium_effect", rhsAlwaysYes MsgIncreaseLegitimacyMediumEffect)
        ,("increase_legitimacy_small_effect" , rhsAlwaysYes MsgIncreaseLegitimacySmallEffect)
        ,("is_imperial_modifier"   , rhsAlwaysYes MsgIsImperialMod)
        ,("is_janissary_modifier"  , rhsAlwaysYes MsgIsJanissaryMod)
        ,("is_rajput_modifier"     , rhsAlwaysYes MsgIsRajputMod)
        ,("is_subject_other_than_tributary_trigger" , rhsAlwaysYes MsgIsSubjectOtherThanTributary)
        ,("kill_ruler"             , rhsAlwaysYes MsgRulerDies)
        ,("may_agitate_for_liberty", rhsAlwaysYes MsgMayAgitateForLiberty) -- Espionage: Destabilizing Efforts
        ,("may_explore"            , rhsAlwaysYes MsgMayExplore) -- Exploration: Quest for the New World
        ,("may_infiltrate_administration", rhsAlwaysYes MsgMayInfiltrateAdministration) -- Espionage: Espionage
        ,("may_sabotage_reputation", rhsAlwaysYes MsgMaySabotageReputation) -- Espionage: Rumormongering
        ,("may_sow_discontent"     , rhsAlwaysYes MsgMaySowDiscontent) -- Espionage: Destabilizing Efforts
        ,("may_study_technology"   , rhsAlwaysYes MsgMayStudyTech) -- Espionage: Shady Recruitment
        ,("move_capital_effect"    , rhsAlwaysYes MsgMoveCapitalEffect)
        ,("prev_move_capital_effect" , rhsAlwaysYes MsgPrevMoveCapitalEffect)
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
        ,("remove_cardinal"          , rhsAlwaysYes MsgLoseCardinal)
        ,("remove_consort"           , rhsAlwaysYes MsgRemoveConsort)
        ,("remove_current_leader"    , rhsAlwaysYes MsgRemoveCurrentLeader)
        ,("remove_non_electors_emperors_from_empire_effect", rhsAlwaysYes MsgLeaveHRE)
        ,("retire_country_leader"       , rhsAlwaysYes MsgRetireCountryLeader)
        ,("same_govt_as_root_trigger" , rhsAlwaysYes MsgSameGovtAsRoot)
        ,("sea_repair"             , rhsAlwaysYes MsgGainSeaRepair) -- Full Maritime
        ,("set_estate_led_regency_privilege" , rhsAlways "random" MsgSetEstateLedRegencyPrivilegeRandom) -- Only random used in 1.31.3
        ,("swap_free_idea_group"     , rhsAlwaysYes MsgSwapFreeIdeaGroup)
        ,("swap_non_generic_missions" , rhsAlwaysYes MsgGainNewMissions)
        ,("auto_explore_adjacent_to_colony", rhsAlwaysYes MsgAutoExploreAdjacentToColony)
        ,("can_fabricate_for_vassals", rhsAlwaysYes MsgCanFabricateForVassals)
        ,("idea_claim_colonies"    , rhsAlwaysYes MsgIdeaClaimColonies)
        ,("may_establish_frontier" , rhsAlwaysYes MsgMayEstablishFrontier)
        ,("may_perform_slave_raid" , rhsAlwaysYes MsgMayPerformSlaveRaid)
        ,("may_perform_slave_raid_on_same_religion", rhsAlwaysYes MsgMayPerformSlaveRaidOnSameReligion)
        ,("may_recruit_female_generals", rhsAlwaysYes MsgMayRecruitFemaleGenerals)
        ,("no_religion_penalty"    , rhsAlwaysYes MsgNoReligionPenalty)
        ,("map_setup"              , rhsAlways "map_setup_random" MsgMapSetupRandom) -- In 1.30.6 all uses are "map_setup = map_setup_random"
        ,("was_never_german_regional_tag_trigger" , rhsAlwaysYes MsgWasNeverGermanReigionalTag)

        -- Various effects from from common/scripted_effects/01_scripted_effects_for_simple_bonuses_penalties.txt
        --
        -- Curia effects
        ,("add_curia_treasur_small_effect" {-sic-} , rhsAlwaysYes (MsgAddCuriaTreasury 25))
        ,("add_curia_treasury_medium_effect"       , rhsAlwaysYes (MsgAddCuriaTreasury 50))
        ,("add_curia_treasury_big_effect"          , rhsAlwaysYes (MsgAddCuriaTreasury 100))
        ,("add_curia_treasury_huge_effect"         , rhsAlwaysYes (MsgAddCuriaTreasury 1000))
        ,("reduce_curia_treasury_medium_effect"    , rhsAlwaysYes (MsgReduceCuriaTreasury (-50)))
        ,("reduce_curia_treasury_big_effect"       , rhsAlwaysYes (MsgReduceCuriaTreasury (-100)))
        ,("reduce_curia_treasury_huge_effect"      , rhsAlwaysYes (MsgReduceCuriaTreasury (-1000)))

        -- Religious currency
        ,("increase_religious_currency_effect"     , rhsAlwaysYes MsgIncreaseReligiousCurrencyEffect)
        ,("reduce_religious_currency_effect"       , rhsAlwaysYes MsgReduceReligiousCurrencyEffect)
        ]

-- | Handlers for numeric statements
handlersNumeric :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
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
        ,("add_threat"                       , numeric MsgAddThreat)
        ,("add_trade_node_income"            , numeric MsgAddTradeNodeIcome)
        ,("army_size_percentage"             , numeric MsgArmySizePc) -- Inti?
        ,("authority"                        , numeric MsgAuth) -- Inti?
        ,("change_government_reform_progress", numeric MsgChangeGovernmentReformProgress)
        ,("change_native_size"               , numeric MsgChangeNativeSize)
        ,("change_siege"                     , numeric MsgGainSiegeProgress)
        ,("colonysize"                       , numeric MsgColonySettlers)
        ,("consort_age"                      , numeric MsgConsortAge)
        ,("convert_heir_to_general"          , numeric (MsgConvertHeirGeneral False))
        ,("convert_female_heir_to_general"   , numeric (MsgConvertHeirGeneral True))
        ,("convert_female_ruler_to_general"  , numeric MsgConvertFemaleRulerGeneral)
        ,("crown_land_share"                 , numeric MsgCrownlandShare)
        ,("curia_treasury_income"            , numeric MsgCuriaTreasuryIncome)
        ,("curia_treasury_size"              , numeric MsgCuriaTreasurySize)
        ,("extend_regency"                   , numeric MsgExtendRegency)
        ,("estate_led_regency_influence"     , numeric MsgEstateLedRegencyInfluence)
        ,("estate_led_regency_loyalty"       , numeric MsgEstateLedRegencyLoyalty)
        ,("federation_size"                  , numeric MsgFederationSize)
        ,("had_recent_war"                   , numeric MsgWasAtWar)
        ,("hegemon_strength"                 , numeric MsgHegemonStrength)
        ,("heir_age"                         , numeric MsgHeirAge)
        ,("hre_size"                         , numeric (\s -> if s == 1 then MsgHREExists else MsgHRESize s))
        ,("is_year"                          , numeric MsgYearIs)
        ,("janissary_percentage"             , numeric MsgJanissaryPercentage)
        ,("legitimacy_equivalent"            , numeric MsgLegitimacyEquivalent)
        ,("monarch_lifespan"                 , numeric MsgMonarchLifespan)
        ,("months_since_defection"           , numeric MsgMonthsSinceDefection)
        ,("native_size"                      , numeric MsgNativeSize)
        ,("num_federation_advancements"      , numeric MsgNumFederationAdvancements)
        ,("num_free_building_slots"          , numeric MsgNumFreeBuildingSlots)
        ,("num_of_aspects"                   , numeric MsgNumAspects)
        ,("num_of_buildings_in_province"     , numeric MsgNumBuildings)
        ,("num_of_centers_of_trade"          , numeric MsgNumCentersOfTrade)
        ,("num_of_colonial_subjects"         , numeric MsgNumColonialSubjects)
        ,("num_of_colonies"                  , numeric MsgNumColonies)
        ,("num_of_diplomatic_relations"      , numeric MsgNumDiplomaticRelations)
        ,("num_of_electors"                  , numeric MsgNumElectors)
        ,("num_of_foreign_hre_provinces"     , numeric MsgNumOfForeignHREProvinces)
        ,("num_of_harmonized"                , numeric MsgNumHarmonized)
        ,("num_of_hired_mercenary_companies" , numeric MsgNumOfHiredMercCompanies)
        ,("num_of_janissaries"               , numeric MsgNumJanissaries)
        ,("num_of_loans"                     , numeric MsgNumLoans)
        ,("num_of_mercenaries"               , numeric MsgNumMercs)
        ,("num_of_non_tributary_subjects"    , numeric MsgNumNonTribSubjects)
        ,("num_of_owned_and_controlled_institutions" , numeric MsgNumOwnInstitutionProvinces)
        ,("num_of_ports"                     , numeric MsgNumPorts) -- same as num_of_total_ports?
        ,("num_of_rajput"                    , numeric MsgNumRajput)
        ,("num_of_rebel_armies"              , numeric MsgNumRebelArmies)
        ,("num_of_rebel_controlled_provinces", numeric MsgNumRebelControlledProvinces)
        ,("num_of_streltsy"                  , numeric MsgNumStreltsy)
        ,("num_of_subjects"                  , numeric MsgNumSubjects)
        ,("num_of_times_improved"            , numeric MsgNumTimesImproved)
        ,("num_of_times_improved_by_owner"   , numeric MsgNumTimesImprovedByOwner)
        ,("num_of_times_used_transfer_development" , numeric MsgNumTimesUsedTransferDevelopment)
        ,("num_of_total_ports"               , numeric MsgNumPorts) -- same as num_of_ports?
        ,("num_of_trade_embargos"            , numeric MsgNumEmbargoes)
        ,("num_of_trusted_allies"            , numeric MsgNumTrustedAllies)
        ,("num_of_unlocked_cults"            , numeric MsgNumUnlockedCults)
        ,("num_of_war_reparations"           , numeric MsgNumOfWarReparations)
        ,("percentage_backing_issue"         , numeric MsgPctBackingParliamentIssue)
        ,("reform_level"                     , numeric MsgReformLevel)
        ,("revolt_percentage"                , numeric MsgRevoltPercentage)
        ,("ruler_age"                        , numeric MsgRulerAge)
        ,("tech_difference"                  , numeric MsgTechDifference)
        ,("trade_company_size"               , numeric MsgTradeCompanySize)
        ,("trade_income_percentage"          , numeric MsgTradeIncomePercentage)
        ,("tributary_state"                  , numeric MsgNumTributaryStates)
        ,("units_in_province"                , numeric MsgUnitsInProvince)
        ,("vassal"                           , numeric MsgHasNumVassals)
        ,("yearly_doom_reduction"            , numeric MsgYearlyDoomReduction)
        -- Special cases
        ,("legitimacy_or_horde_unity"        , numeric MsgLegitimacyOrHordeUnity)
        ]

-- | Handlers for numeric statements that compare
handlersNumericCompare :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericCompare = Tr.fromList
        [("enemies_strength_ratio"           , numericCompare MsgEnemiesStrengthRatio)
        ,("has_manpower"                     , numericCompare MsgHasManpower)
        ,("surrender_progress"               , numericCompare MsgSurrenderProgress)
        ,("threat"                           , numericCompare MsgThreat)
        ,("fascism"                          , numericCompare MsgFascismCompare)
        ,("democratic"                       , numericCompare MsgDemocraticCompare)
        ,("communism"                        , numericCompare MsgCommunismCompare)
        ,("neutrality"                       , numericCompare MsgNeutralityCompare)
        ]

-- | Handlers for numeric statements with icons
handlersNumericIcons :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumericIcons = Tr.fromList
        [("absolutism"               , numericIcon "absolutism" MsgAbsolutism)
        ,("add_absolutism"           , numericIcon "absolutism" MsgGainAbsolutism)
        ,("add_adm_power"            , numericIcon "adm" MsgGainADM)
        ,("add_army_professionalism" , numericIcon "army professionalism" MsgGainArmyProfessionalism)
        ,("add_army_tradition"       , numericIcon "army tradition" MsgGainAT)
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
        ,("add_manpower"             , numericIconLoc "manpower" "MANPOWER" MsgGainManpower)
        ,("add_mercantilism"         , numericIcon "mercantilism" MsgGainMercantilism)
        ,("add_meritocracy"          , numericIcon "meritocracy" MsgGainMeritocracy)
        ,("add_mil_power"            , numericIcon "mil" MsgGainMIL)
        ,("add_militarised_society"  , numericIcon "militarization of state" MsgGainMilitarization)
        ,("add_navy_tradition"       , numericIcon "navy tradition" MsgGainNavyTradition)
        ,("add_papal_influence"      , numericIcon "papal influence" MsgGainPapalInfluence)
        ,("add_patriarch_authority"  , numericIcon "patriarch authority" MsgGainPatAuth)
        ,("add_piety"                , numericIconChange "mysticism" "legalism" MsgGainMysticism MsgGainLegalism)
        ,("add_prestige"             , numericIcon "prestige" MsgGainPrestige)
        ,("add_prosperity"           , numericIcon "prosperity" MsgGainProsperity)
        ,("add_political_power"      , numericIcon "Political power" MsgGainPoliticalPower)
        ,("add_reform_desire"        , numericIcon "reform desire" MsgGainReformDesire)
        ,("add_republican_tradition" , numericIcon "republican tradition" MsgGainRepTrad)
        ,("add_revolutionary_zeal"   , numericIcon "revolutionary zeal" MsgGainRevolutionaryZeal)
        ,("add_scaled_imperial_influence" , numericIcon "imperial authority" MsgGainScaledImperialAuthority)
        ,("add_splendor"             , numericIcon "splendor" MsgGainSplendor)
        ,("add_stability"            , numericIconLoc "stability" "STABILITY" MsgGainLocPC)
        ,("add_tariff_value"         , numericIcon "gloabl tariffs" MsgAddTariffValue)
        ,("add_treasury"             , numericIcon "ducats" MsgAddTreasury)
        ,("add_tribal_allegiance"    , numericIcon "tribal allegiance" MsgGainTribalAllegiance)
        ,("add_unrest"               , numericIcon "local unrest" MsgAddLocalUnrest)
        ,("add_war_exhaustion"       , numericIcon "war exhaustion" MsgGainWarExhaustion)
        ,("add_war_support"          , numericIconLoc "war support" "WAR_SUPPORT" MsgGainLocPC)
        ,("add_yearly_manpower"      , numericIcon "manpower" MsgGainYearlyManpower)
        ,("add_yearly_sailors"       , numericIcon "sailors" MsgGainYearlySailors)
        ,("add_years_of_income"      , numericIcon "ducats" MsgAddYearsOfIncome)
        ,("adm"                      , numericIcon "adm" MsgRulerADM)
        ,("adm_power"                , numericIcon "adm" MsgHasADM)
        ,("administrative_efficiency", numericIconBonus "administrative efficiency" MsgAdminEfficiency MsgAdminEfficiencyBonus)
        ,("army_organiser"           , numericIconLoc "army organizer" "army_organiser" MsgHasAdvisorLevel)
        ,("army_reformer"            , numericIconLoc "army reformer" "army_reformer" MsgHasAdvisorLevel)
        ,("army_tradition"           , numericIconBonusAllowTag "army tradition" MsgArmyTradition MsgArmyTraditionAs MsgYearlyArmyTradition)
        ,("artillery_fraction"       , numericIcon "artillery" MsgArtilleryFraction)
        ,("artist"                   , numericIconLoc "artist" "artist" MsgHasAdvisorLevel)
        ,("average_autonomy"         , numericIcon "autonomy" MsgAverageAutonomy)
        ,("average_unrest"           , numericIcon "unrest" MsgAverageUnrest)
        ,("base_manpower"            , numericIcon "manpower" MsgBaseManpower)
        ,("base_production"          , numericIcon "base production" MsgBaseProduction)
        ,("base_tax"                 , numericIcon "base tax" MsgBaseTax)
        ,("blockade"                 , numericIcon "blockade" MsgBlockade)
        ,("cavalry_fraction"         , numericIcon "cavalry" MsgCavalryFraction)
        ,("center_of_trade"          , numericIcon "cot" MsgCenterOfTrade)
        ,("change_adm"               , numericIcon "adm" MsgGainADMSkill)
        ,("change_dip"               , numericIcon "dip" MsgGainDIPSkill)
        ,("change_heir_adm"          , numericIcon "adm" MsgGainHeirADMSkill)
        ,("change_heir_dip"          , numericIcon "dip" MsgGainHeirDIPSkill)
        ,("change_heir_mil"          , numericIcon "mil" MsgGainHeirMILSkill)
        ,("change_innovativeness"    , numericIcon "innovativeness" MsgChangeInnovativeness)
        ,("change_mil"               , numericIcon "mil" MsgGainMILSkill)
        ,("change_native_ferocity"   , numericIcon "ferocity" MsgChangeNativeFerocity)
        ,("change_native_hostileness" , numericIcon "aggressiveness" MsgChangeNativeAggressiveness)
        ,("change_statists_vs_orangists", numericIconChange "republic" "monarchy" MsgStrengthenStatists MsgStrengthenOrangists)
        ,("church_power"             , numericIcon "church power" MsgChurchPower)
        ,("colonial_governor"        , numericIconLoc "colonial governor" "colonial_governor" MsgHasAdvisorLevel)
        ,("colonist_placement_chance", numericIcon "settler chance" MsgSettlerChance)
        ,("commandant"               , numericIconLoc "commandant" "commandant" MsgHasAdvisorLevel)
        ,("consort_adm"              , numericIcon "adm" MsgConsortADM)
        ,("consort_dip"              , numericIcon "dip" MsgConsortDIP)
        ,("consort_mil"              , numericIcon "mil" MsgConsortMIL)
        ,("corruption"               , numericIcon "corruption" MsgCorruption)
        ,("create_colony"            , numericIcon "global settler increase" MsgCreateColony)
        ,("devastation"              , numericIcon "devastation" MsgDevastation)
        ,("development_cost"         , numericIcon "development cost" MsgDevelCost)
        ,("dip"                      , numericIcon "dip" MsgRulerDIP)
        ,("dip_power"                , numericIcon "adm" MsgHasDIP)
        ,("dip_tech"                 , numericIcon "dip tech" MsgDIPTech)
        ,("diplomat"                 , numericIconLoc "diplomat" "diplomat" MsgHasAdvisorLevel)
        ,("embracement_cost"         , numericIcon "embracement cost" MsgEmbracementCost)
        ,("establish_order_cost"     , numericIcon "establish holy order cost" MsgEstablishOrderCost)
        ,("fervor"                   , numericIcon "fervor" MsgFervor)
        ,("fire_damage"              , numericIcon "land fire damage" MsgLandFireDamage)
        ,("fort_level"               , numericIcon "fort level" MsgFortLevel)
        ,("fortification_expert"     , numericIconLoc "military engineer" "fortification_expert" MsgHasAdvisorLevel)
        ,("global_foreign_trade_power", numericIcon "trade power abroad" MsgTradePowerAbroad)
        ,("global_institution_spread", numericIcon "institution spread" MsgInstitutionSpread)
        ,("global_own_trade_power"   , numericIcon "domestic trade power" MsgDomesticTradePower)
        ,("global_prov_trade_power_modifier", numericIcon "provincial trade power modifier" MsgProvTradePowerMod)
        ,("global_tax_income"        , numericIcon "tax income" MsgGlobalTaxIncome)
        ,("gold_income_percentage"   , numericIcon "gold" MsgGoldIncomePercentage)
        ,("government_reform_progress" , numericIcon "reform progress" MsgGovernmentReformProgress)
        ,("grand_captain"            , numericIconLoc "grand captain" "grand_captain" MsgHasAdvisorLevel)
        ,("grown_by_development"     , numericIcon "development" MsgGrownByDevelopment)
        ,("grown_by_states"          , numericIcon "states" MsgGrownByStates)
        ,("heavy_ship_cost"          , numericIcon "heavy ship cost" MsgHeavyShipCost)
        ,("heir_adm"                 , numericIcon "adm" MsgHeirADM)
        ,("heir_chance"              , numericIcon "chance of new heir" MsgHeirChance)
        ,("heir_dip"                 , numericIcon "dip" MsgHeirDIP)
        ,("heir_mil"                 , numericIcon "mil" MsgHeirMIL)
        ,("harmony"                  , numericIcon "harmony" MsgHarmony)
        ,("horde_unity"              , numericIconBonus "horde unity" MsgHordeUnity MsgYearlyHordeUnity)
        ,("imperial_influence"       , numericIcon "imperial authority" MsgImperialAuthority)
        ,("imperial_mandate"         , numericIconBonus "mandate growth modifier" MsgImperialMandate MsgImperialMandateGrowth)
        ,("innovativeness"           , numericIcon "innovativeness" MsgInnovativeness)
        ,("inquisitor"               , numericIconLoc "inquisitor" "inquisitor" MsgHasAdvisorLevel)
        ,("is_defender_of_faith_of_tier" , numericIcon "dotf" MsgIsDotfTier)
        ,("karma"                    , numericIcon "high karma" MsgKarma)
        ,("legitimacy"               , numericIconBonusAllowTag "legitimacy" MsgLegitimacy MsgLegitimacyAs MsgYearlyLegitimacy)
        ,("liberty_desire"           , numericIconBonus "liberty desire" MsgLibertyDesire MsgLibertyDesireModifier)
        ,("light_ship_fraction"      , numericIcon "light ship" MsgLightShipFraction)
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
        ,("local_monthly_devastation", numericIcon "local devastation" MsgLocalMonthlyDevastation)
        ,("local_production_efficiency", numericIcon "local production efficiency" MsgLocalProdEff)
        ,("local_state_maintenance_modifier", numericIcon "local state maintenance modifier" MsgLocalStateMaintMod)
        ,("local_tax_modifier"       , numericIcon "local tax modifier" MsgLocalTaxMod)
        ,("local_unrest"             , numericIcon "local unrest" MsgLocalUnrest)
        ,("manpower"                 , numericIcon "manpower" MsgManpower)
        ,("manpower_percentage"      , numericIcon "manpower" MsgManpowerPercentage)
        ,("master_of_mint"           , numericIconLoc "master of mint" "master_of_mint" MsgHasAdvisorLevel)
        ,("max_absolutism"           , numericIcon "max absolutism" MsgMaxAbsolutism)
        ,("max_sailors"              , numericIcon "sailors" MsgMaxSailors)
        ,("mercantilism"             , numericIcon "mercantilism" MsgMercantilism)
        ,("mercenary_discipline"     , numericIcon "mercenary discipline" MsgMercenaryDiscipline)
        ,("meritocracy"              , numericIconBonus "meritocracy" MsgMeritocracy MsgYearlyMeritocracy)
        ,("mil"                      , numericIcon "mil" MsgRulerMIL)
        ,("mil_power"                , numericIcon "adm" MsgHasMIL)
        ,("mil_tech"                 , numericIcon "mil tech" MsgMILTech)
        ,("monthly_adm"              , numericIcon "monthly administrative power" MsgMonthlyADM)
        ,("monthly_dip"              , numericIcon "monthly diplomatic power" MsgMonthlyDIP)
        ,("monthly_mil"              , numericIcon "monthly military power" MsgMonthlyMIL)
        ,("mr_aristocrats_influence" , numericIcon "aristocrats influence" MsgAristocratsInfluence)
        ,("nationalism"              , numericIcon "years of separatism" MsgSeparatism)
        ,("native_ferocity"          , numericIcon "ferocity" MsgNativeFerocity)
        ,("native_hostileness"       , numericIcon "aggressiveness" MsgNativeAggressiveness)
        ,("natural_scientist"        , numericIconLoc "natural scientist" "natural_scientist" MsgHasAdvisorLevel)
        ,("naval_forcelimit"         , numericIcon "naval force limit" MsgNavalForcelimit)
        ,("naval_reformer"           , numericIconLoc "naval reformer" "naval_reformer" MsgHasAdvisorLevel)
        ,("navigator"                , numericIconLoc "navigator" "navigator" MsgHasAdvisorLevel)
        ,("navy_reformer"            , numericIconLoc "naval reformer"  "naval_reformer"MsgHasAdvisorLevel) -- both are used
        ,("navy_size_percentage"     , numericIcon "naval force limit" MsgNavyPercentage)
        ,("navy_tradition"           , numericIconBonus "navy tradition" MsgNavyTradition MsgYearlyNavyTradition)
        ,("num_accepted_cultures"    , numericIcon "max promoted cultures" MsgMaxPromotedCultures)
        ,("num_of_admirals"          , numericIcon "admiral" MsgNumAdmirals)
        ,("num_of_allies"            , numericIcon "alliance" MsgNumAllies)
        ,("num_of_banners"           , numericIcon "banners" MsgNumOfBanners)
        ,("num_of_cardinals"         , numericIcon "cardinal" MsgNumCardinals)
        ,("num_of_colonists"         , numericIcon "colonists" MsgNumColonists)
        ,("num_of_conquistadors"     , numericIcon "conquistador" MsgNumOfConquistadors)
        ,("num_of_explorers"         , numericIcon "explorer" MsgNumOfExplorers)
        ,("num_of_generals"          , numericIcon "general" MsgNumGenerals)
        ,("num_of_generals_with_traits" , numericIcon "general" MsgNumGeneralsWithTrait)
        ,("num_of_merchants"         , numericIcon "merchant" MsgNumMerchants)
        ,("num_of_missionaries"      , numericIcon "missionary" MsgNumMissionaries)
        ,("num_of_royal_marriages"   , numericIcon "royal marriage" MsgNumRoyalMarriages)
        ,("num_of_states"            , numericIcon "states" MsgNumOfStates)
        ,("num_of_unions"            , numericIcon "personal union" MsgNumUnions)
        ,("num_of_vassals"           , numericIcon "vassal" MsgNumVassals) -- includes other subjects?
        ,("overextension_percentage" , numericIcon "overextension" MsgOverextension)
        ,("philosopher"              , numericIconLoc "philosopher" "philosopher" MsgHasAdvisorLevel)
        ,("power_projection"         , numericIcon "power projection" MsgPowerProjection)
        ,("province_trade_power"     , numericIcon "trade power" MsgProvinceTradePower)
        ,("province_trade_power_modifier", numericIcon "local trade power modifier" MsgLocalTradePowerMod)
        ,("province_trade_power_value", numericIcon "local trade power" MsgLocalTradePower)
        ,("quartermaster"            , numericIconLoc "quartermaster" "quartermaster" MsgHasAdvisorLevel)
        ,("raze_power_gain"          , numericIcon "razing power gain" MsgRazingPowerGain)
        ,("recover_navy_morale_speed", numericIcon "recover navy morale speed" MsgRecoverNavyMoraleSpeed)
        ,("recruitmaster"            , numericIconLoc "master recruiter" "recruitmaster" MsgHasAdvisorLevel)
        ,("revolutionary_zeal"       , numericIcon "revolutionary zeal" MsgRevolutionaryZeal)
        ,("share_of_starting_income" , numericIcon "income" MsgShareOfStartingIncome)
        ,("spymaster"                , numericIconLoc "spymaster" "spymaster" MsgHasAdvisorLevel)
        ,("total_base_tax"           , numericIcon "base tax" MsgTotalBaseTax)
        ,("treasurer"                , numericIconLoc "treasurer" "treasurer" MsgHasAdvisorLevel)
        ,("yearly_corruption_increase" , numericIcon "yearly corruption" MsgYearlyCorruptionIncrease)
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
        --general modifiers
        ,("monthly_population"       , numericLoc "MODIFIER_MONTHLY_POPULATION" MsgModifier) -- % neutral?
        ,("nuclear_production_factor" , numericLoc "MODIFIER_NUCLEAR_PRODUCTION_FACTOR" MsgModifier) -- % pos
        ,("research_sharing_per_country_bonus" , numericLoc "MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS" MsgModifier) -- flat pos
        ,("research_sharing_per_country_bonus_factor" , numericLoc "MODIFIER_RESEARCH_SHARING_PER_COUNTRY_BONUS_FACTOR" MsgModifier) -- % pos
        ,("research_speed_factor"    , numericLoc "MODIFIER_RESEARCH_SPEED_FACTOR" MsgModifier) -- % pos
        ,("local_resources_factor"   , numericLoc "MODIFIER_LOCAL_RESOURCES_FACTOR" MsgModifier) -- % pos
        -- Politics modifiers
        ,("min_export"               , numericLoc "MODIFIER_MIN_EXPORT_FACTOR" MsgModifier) -- flat neutral?
        ,("trade_opinion_factor"     , numericLoc "MODIFIER_" MsgModifier) -- % pos
        ,("party_popularity_stability_factor" , numericLoc " MODIFIER_STABILITY_POPULARITY_FACTOR" MsgModifier) -- % pos
        ,("political_power_cost"     , numericLoc "MODIFIER_POLITICAL_POWER_COST" MsgModifier) -- flat neg
        ,("political_power_gain"     , numericLoc "MODIFIER_POLITICAL_POWER_GAIN" MsgModifier) -- flat pos
        ,("political_power_factor"   , numericLoc "MODIFIER_POLITICAL_POWER_FACTOR" MsgModifier) -- % pos
        ,("stability_factor"         , numericLoc "MODIFIER_STABILITY_FACTOR" MsgModifier) -- flat pos
        ,("stability_weekly"         , numericLoc "MODIFIER_STABILITY_WEEKLY" MsgModifier) -- flat pos
        ,("stability_weekly_factor"  , numericLoc "MODIFIER_STABILITY_WEEKLY_FACTOR" MsgModifier) -- flat pos
        ,("war_support_weekly"       , numericLoc "MODIFIER_WAR_SUPPORT_WEEKLY" MsgModifier) -- flat pos
        ,("war_support_weekly_factor" , numericLoc "MODIFIER_WAR_SUPPORT_WEEKLY_FACTOR" MsgModifier) -- % pos
        ,("drift_defence_factor"     , numericLoc "MODIFIER_DRIFT_DEFENCE_FACTOR" MsgModifier) -- % pos
        -- Diplomacy
        ,("generate_wargoal_tension" , numericLoc "MODIFIER_GENERATE_WARGOAL_TENSION_LIMIT" MsgModifier) -- flat neg
        ,("guarantee_cost"           , numericLoc "MODIFIER_GUARANTEE_COST" MsgModifier) -- % neg
        ,("guarantee_tension"        , numericLoc "MODIFIER_GUARANTEE_TENSION_LIMIT" MsgModifier) -- flat neg
        ,("join_faction_tension"     , numericLoc "MODIFIER_JOIN_FACTION_TENSION_LIMIT" MsgModifier) -- flat neg
        ,("justify_war_goal_time"    , numericLoc "MODIFIER_JUSTIFY_WAR_GOAL_TIME" MsgModifier) -- % neg
        ,("lend_lease_tension"       , numericLoc " MODIFIER_LEND_LEASE_TENSION_LIMIT" MsgModifier) -- flat neg
        ,("opinion_gain_monthly"     , numericLoc "MODIFIER_OPINION_GAIN_MONTHLY" MsgModifier) -- flat pos
        ,("request_lease_tension"    , numericLoc "MODIFIER_REQUEST_LEASE_TENSION_LIMIT" MsgModifier) -- % neg
        ,("surrender_limit"          , numericLoc "MODIFIER_SURRENDER_LIMIT" MsgModifier) -- % neutr?
        -- Equipment
        ,("equipment_conversion_speed" , numericLoc "EQUIPMENT_CONVERSION_SPEED_MODIFIERS" MsgModifier) -- % neg
        ,("equipment_upgrade_xp_cost" , numericLoc "MODIFIER_EQUIPMENT_UPGRADE_XP_COST" MsgModifier) -- % neg
        ,("production_factory_efficiency_gain_factor" , numericLoc "MODIFIER_PRODUCTION_FACTORY_EFFICIENCY_GAIN_FACTOR" MsgModifier) -- % pos
        ,("production_factory_max_efficiency_factor" , numericLoc "MODIFIER_PRODUCTION_FACTORY_MAX_EFFICIENCY_FACTOR" MsgModifier) -- % pos
        ,("production_factory_start_efficiency_factor" , numericLoc "MODIFIER_PRODUCTION_FACTORY_START_EFFICIENCY_FACTOR" MsgModifier) -- % pos
        -- Military outside of combat
        ,("command_power_gain"       , numericLoc "MODIFIER_COMMAND_POWER_GAIN" MsgModifier) -- flat pos
        ,("command_power_gain_mult"  , numericLoc "MODIFIER_COMMAND_POWER_GAIN_MULT" MsgModifier) -- % neg
        ,("conscription"             , numericLoc "MODIFIER_CONSCRIPTION_FACTOR" MsgModifier) -- flat neut
        ,("conscription_factor"      , numericLoc "MODIFIER_CONSCRIPTION_TOTAL_FACTOR" MsgModifier) -- % pos
        ,("max_command_power"        , numericLoc "MODIFIER_MAX_COMMAND_POWER" MsgModifier) -- flat pos
        ,("max_command_power_mult"   , numericLoc "MODIFIER_MAX_COMMAND_POWER_MULT" MsgModifier)  -- % pos
        ,("weekly_manpower"          , numericLoc "MODIFIER_WEEKLY_MANPOWER" MsgModifier) -- flat pos
        -- Fuel
        ,("base_fuel_gain"           , numericLoc "MODIFIER_BASE_FUEL_GAIN_ADD" MsgModifier) -- flat neutr?
        ,("base_fuel_gain_factor"    , numericLoc "MODIFIER_BASE_FUEL_GAIN_FACTOR" MsgModifier) -- % pos
        ,("fuel_cost"                , numericLoc "MODIFIER_FUEL_COST" MsgModifier) -- flat neg
        ,("fuel_gain"                , numericLoc "MODIFIER_FUEL_GAIN_ADD" MsgModifier) -- flat pos
        ,("fuel_gain_factor"         , numericLoc "MODIFIER_MAX_FUEL_FACTOR" MsgModifier) -- flat pos
        ,("max_fuel"                 , numericLoc "MODIFIER_MAX_FUEL_ADD" MsgModifier) -- flat
        ,("max_fuel_factor"          , numericLoc "MODIFIER_MAX_FUEL_FACTOR" MsgModifier) -- % pos
        -- buildings
        ,("civilian_factory_use"     , numericLoc "MODIFIER_CIVILIAN_FACTORY_USE" MsgModifier) -- flat neut
        ,("consumer_goods_factor"    , numericLoc "MODIFIER_CONSUMER_GOODS_FACTOR" MsgModifier) -- flat percentage? neutr
        ,("conversion_cost_civ_to_mil_factor" , numericLoc "MODIFIER_CONVERSION_COST_CIV_TO_MIL_FACTOR" MsgModifier) -- % neutr
        ,("conversion_cost_mil_to_civ_factor" , numericLoc "MODIFIER_CONVERSION_COST_MIL_TO_CIV_FACTOR" MsgModifier) -- % neutr
        ,("global_building_slots"    , numericLoc "MODIFIER_GLOBAL_BUILDING_SLOTS" MsgModifier) -- flat
        ,("global_building_slots_factor" , numericLoc "MODIFIER_GLOBAL_BUILDING_SLOTS_FACTOR" MsgModifier) -- %
        ,("industrial_capacity_dockyard" , numericLoc "MODIFIER_INDUSTRIAL_CAPACITY_DOCKYARD_FACTOR" MsgModifier) -- %
        ,("industrial_capacity_factory" , numericLoc "MODIFIER_INDUSTRIAL_CAPACITY_FACTOR" MsgModifier) -- %
        ,("industry_repair_factor"   , numericLoc "MODIFIER_INDUSTRY_REPAIR_FACTOR" MsgModifier) -- %
        ,("line_change_production_efficiency_factor" , numericLoc "MODIFIER_LINE_CHANGE_PRODUCTION_EFFICIENCY_FACTOR" MsgModifier) -- %
        ,("production_oil_factor"    , numericLoc "MODIFIER_PRODUCTION_OIL_FACTOR" MsgModifier) -- %
        ,("production_speed_buildings_factor" , numericLoc "MODIFIER_PRODUCTION_SPEED_BUILDINGS_FACTOR" MsgModifier) -- %
        -- resistance and compliance
        ,("civilian_intel_to_others" , numericLoc "MODIFIER_CIVILIAN_INTEL_TO_OTHERS" MsgModifier) -- flat neg
        -- AI
        -- General Combat
        ,("offence"                  , numericLoc "MODIFIER_OFFENCE" MsgModifier) -- % pos
        ,("defence"                  , numericLoc "MODIFIER_DEFENCE" MsgModifier) -- % pos
        -- Land Combat
        ,("army_attack_factor"       , numericLoc "MODIFIERS_ARMY_ATTACK_FACTOR" MsgModifier) -- % pos
        -- Naval combat
        -- Air combat
        ]

-- | Handlers for statements pertaining to modifiers
handlersModifiers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersModifiers = Tr.fromList
        [--("add_country_modifier"           , addModifier MsgCountryMod)
--        ,("add_disaster_modifier"          , addModifier MsgDisasterMod)
--        ,("add_permanent_province_modifier", addModifier MsgPermanentProvMod)
--        ,("add_province_modifier"          , addModifier MsgProvMod)
--        ,("add_ruler_modifier"             , addModifier MsgRulerMod)
--        ,("add_trade_modifier"             , addModifier MsgTradeMod)
         ("add_dynamic_modifier"           , addDynamicModifier)
--        ,("has_country_modifier"           , withLocAtom2 MsgCountryMod MsgHasModifier)
--        ,("has_province_modifier"          , withLocAtom2 MsgProvMod MsgHasModifier)
--        ,("has_ruler_modifier"             , withLocAtom2 MsgRulerMod MsgHasModifier)
--        ,("has_trade_modifier"             , tradeMod)
--        ,("remove_country_modifier"        , withLocAtom2 MsgCountryMod MsgRemoveModifier)
--        ,("remove_province_modifier"       , withLocAtom2 MsgProvMod MsgRemoveModifier)
        ]

-- | Handlers for simple compound statements
handlersCompound :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersCompound = Tr.fromList
        -- Note that "any" can mean "all" or "one or more" depending on context.
        [("and" , compoundMessage MsgAllOf) --AND
        ,("root", compoundMessagePronoun) --ROOT
        ,("from", compoundMessagePronoun) --FROM
        ,("prev", compoundMessagePronoun) --PREV
        -- no THIS, not used on LHS
        ,("not" , compoundMessage MsgNoneOf) --NOT
        ,("or"  , compoundMessage MsgAtLeastOneOf) --OR
        -- Tagged blocks
        ,("event_target", compoundMessageTagged MsgEventTarget (Just HOI4From))
        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>."
        -- triggerscopes
        ,("all_allied_country" {- sic -}, scope HOI4Country     . compoundMessage MsgAllAlliedCountries)
        ,("all_army_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllArmyLeaders)
        ,("all_character"               , scope HOI4Character   . compoundMessage MsgAllCharacters)
        ,("all_controlled_state"        , scope HOI4ScopeState  . compoundMessage MsgAllControlledStates)
        ,("all_core_state"              , scope HOI4ScopeState  . compoundMessage MsgAllCoreStates)
        ,("all_country"        {- sic -}, scope HOI4Country     . compoundMessage MsgAllCountries)
        ,("all_enemy_country"           , scope HOI4Country     . compoundMessage MsgAllEnemyCountries)
        ,("all_guaranteed_country"      , scope HOI4Country     . compoundMessage MsgAllGuaranteedCountries)
        ,("all_navy_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllNavyLeaders)
        ,("all_neighbor_country"        , scope HOI4Country     . compoundMessage MsgAllNeighborCountries)
        ,("all_neighbor_state"          , scope HOI4ScopeState  . compoundMessage MsgAllNeighborStates)
        ,("all_occupied_country"        , scope HOI4Country     . compoundMessage MsgAllOccupiedCountries)
        ,("all_operative_leader"        , scope HOI4Operative   . compoundMessage MsgAllOperativeLeaders)
        ,("all_other_country"           , scope HOI4Country     . compoundMessage MsgAllOtherCountries)
        ,("all_owned_state"             , scope HOI4ScopeState  . compoundMessage MsgAllOwnedStates)
        ,("all_state"                   , scope HOI4ScopeState  . compoundMessage MsgAllStates)
        ,("all_subject_countries"       , scope HOI4Country     . compoundMessage MsgAllSubjectCountries)
        ,("all_unit_leader"             , scope HOI4UnitLeader  . compoundMessage MsgAllUnitLeaders)
--        ,("any_allied_country
--        ,("any_army_leader
--        ,("any_character
--        ,("any_controlled_state
--        ,("any_core_state
--        ,("any_country
--        ,("any_country_with_original_tag
--        ,("any_enemy_country
--        ,("any_guaranteed_country
--        ,("any_home_area_neighbor_country
--        ,("any_navy_leader
--        ,("any_neighbor_country
--        ,("any_neighbor_state
--        ,("any_occupied_country
--        ,("any_operative_leader
--        ,("any_other_country
--        ,("any_owned_state
--        ,("any_state
--        ,("any_subject_country
--        ,("any_unit_leader

        ,("any_active_trade_node"   , scope HOI4TradeNode . compoundMessage MsgAnyActiveTradeNode)
        ,("any_ally"                , scope HOI4Country   . compoundMessage MsgAnyAlly)
        ,("any_army"                ,                      compoundMessage MsgAnyArmy)
        ,("any_core_country"        , scope HOI4Country   . compoundMessage MsgAnyCoreCountry) -- used in province scope
        ,("any_core_province"       , scope HOI4ScopeState  . compoundMessage MsgAnyCoreProvince)
        ,("any_country"             , scope HOI4Country   . compoundMessage MsgAnyCountry)
        ,("any_country_active_in_node" , scope HOI4Country . compoundMessage MsgAnyCountryActiveInNode)
        ,("any_empty_neighbor_province", scope HOI4ScopeState  . compoundMessage MsgAnyEmptyNeighborProvince)
        ,("any_enemy_country"       , scope HOI4Country   . compoundMessage MsgAnyEnemyCountry)
        ,("any_heretic_province"    , scope HOI4ScopeState  . compoundMessage MsgAnyHereticProvince)
        ,("any_hired_mercenary_company" ,                  compoundMessage MsgAnyHiredMercenaryCompany) -- TOOD: Need unit scope?
        ,("any_known_country"       , scope HOI4Country   . compoundMessage MsgAnyKnownCountry)
        ,("any_neighbor_country"    , scope HOI4Country   . compoundMessage MsgAnyNeighborCountry)
        ,("any_neighbor_province"   , scope HOI4ScopeState  . compoundMessage MsgAnyNeighborProvince)
        ,("any_owned_province"      , scope HOI4ScopeState  . compoundMessage MsgAnyOwnedProvince)
        ,("any_privateering_country", scope HOI4TradeNode . compoundMessage MsgAnyPrivateeringCountry)
        ,("any_province"            , scope HOI4ScopeState  . compoundMessage MsgAnyProvince)
        ,("any_province_in_state"   , scope HOI4ScopeState  . compoundMessage MsgAnyProvinceInState)
        ,("any_rival_country"       , scope HOI4Country   . compoundMessage MsgAnyRival)
        ,("any_subject_country"     , scope HOI4Country   . compoundMessage MsgAnySubject)
        ,("any_trade_node"          , scope HOI4TradeNode . compoundMessage MsgAnyTradeNode)
        ,("any_trade_node_member_country" , scope HOI4Country . compoundMessage MsgAnyTradeNodeCountry)
        ,("any_trade_node_member_province" , scope HOI4ScopeState . compoundMessage MsgAnyTradeNodeProvince)
        ,("area_for_scope_province" , scope HOI4ScopeState  . scopeProvince MsgAreaOfProvince MsgAreaOfProvinceAll)

        ,("capital_scope"           , scope HOI4ScopeState  . compoundMessage MsgCapital)
        ,("colonial_parent"         , scope HOI4Country   . compoundMessage MsgColonialParent)
        ,("controller"              , scope HOI4Country   . compoundMessage MsgController)
        ,("else"                    ,                      compoundMessage MsgElse)
        ,("else_if"                 ,                      compoundMessage MsgElseIf)
        ,("emperor"                 , scope HOI4Country   . compoundMessage MsgEmperor)
        ,("every_active_trade_node" , scope HOI4TradeNode . compoundMessage MsgEveryActiveTradeNode)
        ,("every_ally"              , scope HOI4TradeNode . compoundMessage MsgEveryAlly)
        ,("every_core_country"      , scope HOI4Country   . compoundMessage MsgEveryCoreCountry) -- used in province scope
        ,("every_core_province"     , scope HOI4ScopeState  . compoundMessage MsgEveryCoreProvince)
        ,("every_country"           , scope HOI4Country   . compoundMessage MsgEveryCountry)
        ,("every_other_country"     , scope HOI4Country   . compoundMessage MsgEveryOtherCountry)
        ,("every_enemy_country"     , scope HOI4Country   . compoundMessage MsgEveryEnemyCountry)
        ,("every_federation_member" , scope HOI4Country   . compoundMessage MsgEveryFederationMember)
        ,("every_heretic_province"  , scope HOI4ScopeState  . compoundMessage MsgEveryHereticProvince)
        ,("every_known_country"     , scope HOI4Country   . compoundMessage MsgEveryKnownCountry)
        ,("every_neighbor_country"  , scope HOI4Country   . compoundMessage MsgEveryNeighborCountry)
        ,("every_neighbor_province" , scope HOI4ScopeState  . compoundMessage MsgEveryNeighborProvince)
        ,("every_owned_province"    , scope HOI4ScopeState  . compoundMessage MsgEveryOwnedProvince)
        ,("every_state"             , scope HOI4ScopeState  . compoundMessage MsgEveryState)
        ,("every_rival_country"     , scope HOI4Country   . compoundMessage MsgEveryRival)
        ,("every_subject_country"   , scope HOI4Country   . compoundMessage MsgEverySubject)
        ,("every_trade_node_member_country" , scope HOI4Country . compoundMessage MsgEveryTradeNodeMemberCountry)
        ,("home_province"           ,                      compoundMessage MsgHomeProvince) -- For mercs
        ,("home_trade_node"         , scope HOI4TradeNode . compoundMessage MsgHomeTradeNode)
        ,("home_trade_node_effect_scope" , scope HOI4TradeNode . compoundMessage MsgHomeTradeNodeEffectScope)
        ,("hidden_effect"           ,                      compoundMessage MsgHiddenEffect)
        ,("hidden_trigger"          ,                      compoundMessage MsgHiddenTrigger)
        ,("if"                      ,                      compoundMessage MsgIf) -- always needs editing
        ,("limit"                   , setIsInEffect False . compoundMessage MsgLimit) -- always needs editing
        ,("location"                , scope HOI4ScopeState  . compoundMessage MsgLocation) -- For mercs
        ,("most_province_trade_power", scope HOI4Country  . compoundMessage MsgMostProvinceTradePower)
        ,("overlord"                , scope HOI4Country   . compoundMessage MsgOverlord)
        ,("owner"                   , scope HOI4Country   . compoundMessage MsgOwner)
        ,("random_active_trade_node", scope HOI4TradeNode . compoundMessage MsgRandomActiveTradeNode)
        ,("random_ally"             , scope HOI4Country   . compoundMessage MsgRandomAlly)
        ,("random_Character"        , scope HOI4Character . compoundMessage MsgRandomCharacter)
        ,("random_core_country"     , scope HOI4Country   . compoundMessage MsgRandomCoreCountry)
        ,("random_core_province"    , scope HOI4Country   . compoundMessage MsgRandomCoreProvince)
        ,("random_country"          , scope HOI4Country   . compoundMessage MsgRandomCountry)
        ,("random_elector"          , scope HOI4Country   . compoundMessage MsgRandomElector)
        ,("random_enemy_country"    , scope HOI4Country   . compoundMessage MsgRandomEnemyCountry)
        ,("random_empty_neighbor_province", scope HOI4ScopeState . compoundMessage MsgRandomEmptyNeighborProvince)
        ,("random_heretic_province"    , scope HOI4ScopeState  . compoundMessage MsgRandomHereticProvince)
        ,("random_hired_mercenary_company" ,                compoundMessage MsgRandomHiredMercenaryCompany) -- TODO: Need unit scope?
        ,("random_known_country"    , scope HOI4Country   . compoundMessage MsgRandomKnownCountry)
        ,("random_neighbor_country" , scope HOI4Country   . compoundMessage MsgRandomNeighborCountry)
        ,("random_neighbor_province", scope HOI4ScopeState  . compoundMessage MsgRandomNeighborProvince)
        ,("random_owned_area"       , scope HOI4ScopeState  . compoundMessage MsgRandomOwnedArea)
        ,("random_owned_state"      , scope HOI4ScopeState  . compoundMessage MsgRandomOwnedState)
        ,("random_owned_controlled_state", scope HOI4ScopeState  . compoundMessage MsgRandomOwnedControlledState)
        ,("random_other_country"    , scope HOI4Country   . compoundMessage MsgRandomOtherCountry)
        ,("random_privateering_country", scope HOI4TradeNode . compoundMessage MsgRandomPrivateeringCountry)
        ,("random_state"            , scope HOI4ScopeState  . compoundMessage MsgRandomState)
        ,("random_rival_country"    , scope HOI4Country   . compoundMessage MsgRandomRival)
        ,("random_subject_country"  , scope HOI4Country   . compoundMessage MsgRandomSubjectCountry)
        ,("random_trade_node"       , scope HOI4TradeNode . compoundMessage MsgRandomTradeNode)
        ,("random_trade_node_member_province" , scope HOI4ScopeState . compoundMessage MsgRandomTradeNodeMemberProvince)
        ,("region_for_scope_province" , scope HOI4ScopeState . scopeProvince MsgRegionProvinceScope MsgRegionProvinceScopeAll)
        ,("strongest_trade_power"   , scope HOI4Country   . compoundMessage MsgStrongestTradePower) -- always needs editing
        ,("variable_arithmetic_trigger" ,                  compoundMessage MsgVariableArithmeticTrigger)
        ,("while"                   , scope HOI4Country   . compoundMessage MsgWhile) -- always needs editing
        ]

withLocAtomTitle msg = withLocAtom' msg (\t -> t <> "_title")
withLocAtomName msg = withLocAtom' msg (\t -> t <> "_name")

-- | Handlers for simple statements where RHS is a localizable atom
handlersLocRhs :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersLocRhs = Tr.fromList
        [("add_great_project"     , withLocAtom MsgStartConstructingGreatProject)
        ,("add_government_reform" , withLocAtom MsgAddGovernmentReform)
        ,("can_be_overlord"       , withLocAtom MsgCanBeOverlord)
        ,("change_government"     , withLocAtom MsgChangeGovernment)
        ,("set_state_name"        , withLocAtom MsgSetStateName)
        ,("set_state_category"    , withLocAtom MsgSetStateCategory)
        ,("colonial_region"       , withLocAtom MsgColonialRegion)
        ,("complete_mission"      , withLocAtomTitle MsgCompleteMission)
        ,("council_position"      , withLocAtom MsgCouncilPosition)
        ,("current_debate"        , withLocAtom MsgCurrentDebate)
        ,("custom_effect_tooltip" , withLocAtom MsgCustomEffectTooltip)
--        ,("end_disaster"          , withLocAtom MsgDisasterEnds)
        ,("end_incident"          , withLocAtomTitle MsgEndIncident)
        ,("government"            , withLocAtom MsgGovernmentIs)
        ,("has_advisor"           , withLocAtom MsgHasAdvisor)
        ,("has_active_policy"     , withLocAtom MsgHasActivePolicy)
        ,("has_construction"      , withLocAtom MsgConstructing)
        ,("has_church_aspect"     , withLocAtom MsgHasChurchAspect)
--        ,("has_disaster"          , withLocAtom MsgDisasterOngoing)
        ,("has_estate_privilege"  , withLocAtom MsgHasEstatePrivilege)
        ,("has_faction"           , withLocAtom MsgHasFaction)
--        ,("has_leader"            , withLocAtom MsgHasLeader) -- will usually fail localization
        ,("has_mission"           , withLocAtomTitle MsgHasMission)
        ,("has_opinion_modifier"  , withLocAtom MsgHasOpinionMod)
        ,("has_reform"            , withLocAtom MsgHasReform)
        ,("has_terrain"           , withLocAtom MsgHasTerrain)
        ,("has_winter"            , withLocAtom MsgHasWinter)
        ,("hre_reform_passed"     , withLocAtomTitle MsgHREPassedReform)
        ,("in_league"             , withLocAtom MsgInLeague)
        ,("is_incident_active"    , withLocAtomTitle MsgIsIncidentActive)
        ,("is_incident_happened"  , withLocAtomTitle MsgHasIncidentHappened)
        ,("is_incident_possible"  , withLocAtomTitle MsgIsIncidentPossible)
        ,("is_in_tech_sharing_group" , withLocAtomName MsgIsInTechSharingGroup)
        ,("add_to_tech_sharing_group" , withLocAtomName MsgAddToTechSharingGroup)
        ,("is_subject_of_type"    , withLocAtomTitle MsgIsSubjectOfType)
        ,("kill_advisor"          , withLocAtom MsgAdvisorDies)
        ,("mission_completed"     , withLocAtomTitle MsgMissionCompleted)
        ,("native_policy"         , withLocAtom MsgNativePolicy)
        ,("override_country_name" , withLocAtom MsgOverrideCountryName)
        ,("remove_advisor"        , withLocAtom MsgLoseAdvisor)
        ,("remove_advisor_by_category" , withLocAtom MsgRemoveAdvisor)
        ,("remove_government_reform" , withLocAtom MsgRemoveGovernmentReform)
        ,("remove_estate_privilege" , withLocAtom MsgRemoveEstatePrivilege)
        ,("rename_capital"        , withLocAtom MsgRenameCapital) -- will usually fail localization
        ,("set_estate_privilege"  , withLocAtom MsgGrantEstatePrivilege)
        ,("set_imperial_incident" , withLocAtom MsgStartHREIncident)
        ,("superregion"           , withLocAtom MsgSuperRegionIs)
        ,("trade_company_region"  , withLocAtom MsgTradeCompanyRegion)
        ]

-- | Handlers for statements whose RHS is a province ID
handlersProvince :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersProvince = Tr.fromList
        [("add_state_core"    , withProvince MsgAddStateCore)
        ,("capital"           , withProvince MsgCapitalIs)
        ,("controls_state"    , withProvince MsgControlsState)
        ,("has_full_control_of_state" , withProvince MsgHasFullControlOfState)
        ,("discover_province" , withProvince MsgDiscoverProvince)
        ,("higher_development_than" , withProvince MsgHigherDevelopmentThan)
        ,("owns_state"              , withProvince MsgOwnsState)
        ,("owns_core_province", withProvince MsgOwnsCore)
        ,("owns_or_non_sovereign_subject_of" , withProvince MsgOwnsOrNonTribSubject)
        ,("owns_or_vassal_of" , withProvince MsgOwnsOrVassal)
        ,("province_id"       , withProvince MsgProvinceIs)
        ,("set_capital"       , withProvince MsgSetCapital)
        ,("transfer_state"    , withProvince MsgTransferState)
        ]

-- | Handlers for statements whose RHS is a flag OR a province ID
-- Also abusable for tag,scope purposes
handlersFlagOrProvince :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersFlagOrProvince = Tr.fromList
        [("add_claim"          , withFlagOrProvince MsgAddClaimFor MsgAddClaimOn)
--        ,("add_core"           , withFlagOrProvince MsgGainCore MsgGainCoreProvince)
        ,("add_permanent_claim", withFlagOrProvince MsgGainPermanentClaimCountry MsgGainPermanentClaimProvince)
        ,("cavalry"            , withFlagOrProvince MsgCavalrySpawnsCountry MsgCavalrySpawnsProvince)
        ,("infantry"           , withFlagOrProvince MsgInfantrySpawnsCountry MsgInfantrySpawnsProvince)
        ,("remove_core"        , withFlagOrProvince MsgLoseCoreCountry MsgLoseCoreProvince)
        ,("is_colonial_nation_of" , withFlagOrProvince MsgIsColonialNationOf MsgIsColonialNationOf)
        -- RHS is a flag or province id, but the statement's meaning depends on the scope
        ,("has_discovered"     , withFlagOrProvinceHOI4Scope MsgHasDiscovered MsgHasDiscovered MsgDiscoveredBy MsgDiscoveredBy) -- scope sensitive
        ,("same_continent"     , withFlagOrProvinceHOI4Scope (MsgSameContinent True True) (MsgSameContinent True False) (MsgSameContinent False True) (MsgSameContinent False False)) -- scope sensitive
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
        [("clr_consort_flag" , withNonlocAtom2 MsgConsortFlag MsgClearFlag)
        ,("clr_country_flag" , withNonlocAtom2 MsgCountryFlag MsgClearFlag)
        ,("clr_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgClearFlag)
        ,("clr_heir_flag"    , withNonlocAtom2 MsgHeirFlag MsgClearFlag)
        ,("clr_state_flag", withNonlocAtom2 MsgStateFlag MsgClearFlag)
        ,("clr_ruler_flag"   , withNonlocAtom2 MsgRulerFlag MsgClearFlag)
        ,("clear_exiled_ruler" , withNonlocAtom MsgClearExiledRuler)
        ,("clear_saved_name" , withNonlocAtom MsgClearSavedName)
        ,("exile_heir_as"    , withNonlocAtom MsgExileHeir)
        ,("exile_ruler_as"   , withNonlocAtom MsgExileRuler)
        ,("exiled_same_dynasty_as_current" , withNonlocAtom MsgExiledRulerSameDynastyAsCurrent)
        ,("has_consort_flag" , withNonlocAtom2 MsgConsortFlag MsgHasFlag)
        ,("has_heir_flag"    , withNonlocAtom2 MsgHeirFlag MsgHasFlag)
        ,("has_state_flag", withNonlocAtom2 MsgStateFlag MsgHasFlag)
        ,("has_ruler"        , withNonlocAtom MsgHasRuler)
        ,("has_ruler_flag"   , withNonlocAtom2 MsgRulerFlag MsgHasFlag)
        ,("has_saved_event_target", withNonlocAtom MsgHasSavedEventTarget)
        ,("save_event_target_as", withNonlocAtom MsgSaveEventTargetAs)
        ,("save_global_event_target_as", withNonlocAtom MsgSaveGlobalEventTargetAs)
        ,("set_consort_flag" , withNonlocAtom2 MsgConsortFlag MsgSetFlag)
        ,("set_cosmetic_tag" , withNonlocAtom MsgSetCosmeticTag)
        ,("set_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgSetFlag)
        ,("set_State_flag", withNonlocAtom2 MsgStateFlag MsgSetFlag)
        ,("set_heir"         , withNonlocAtom MsgSetHeir)
        ,("set_heir_flag"    , withNonlocAtom2 MsgHeirFlag MsgSetFlag)
        ,("set_ruler"        , withNonlocAtom MsgSetRuler)
        ,("set_ruler_flag"   , withNonlocAtom2 MsgRulerFlag MsgSetFlag)
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
        ,("remove_ruler_personality"  , withLocAtomIcon (MsgRemoveRulerPersonality False))
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
        [("add_claim"               , withFlag MsgGainClaim)
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
        ,("give_military_access"    , withFlag MsgGiveMilitaryAccess)
        ,("guaranteed_by"           , withFlag MsgGuaranteedBy)
        ,("has_guaranteed"          , withFlag MsgHasGuaranteed)
        ,("has_merchant"            , withFlag MsgHasMerchant)
        ,("has_most_province_trade_power" , withFlag MsgHasMostProvinceTradePower)
        ,("has_religious_school_of" , withFlag MsgHasReligiousSchoolOf)
        ,("has_trader"              , withFlag MsgHasTrader)
        ,("heavy_ship"              , withFlag MsgHeavyShip)
        ,("historical_friend_with"  , withFlag MsgHistoricalFriendWith)
        ,("historical_rival_with"   , withFlag MsgHistoricalRivalWith)
        ,("inherit"                 , withFlag MsgInherit)
        ,("has_pillaged_capital_against" , withFlag MsgHasPillagedCapitalAgainst)
        ,("humiliated_by"           , withFlag MsgHumiliatedBy)
        ,("is_capital_of"           , withFlag MsgIsCapitalOf)
        ,("is_enemy"                , scope HOI4Country . withFlag MsgIsEnemy)
--        ,("is_in_trade_league_with" , withFlag MsgIsInTradeLeagueWith)
        ,("is_in_faction_with"      , withFlag MsgIsInFactionWith)
        ,("is_league_enemy"         , withFlag MsgIsLeagueEnemy)
        ,("is_league_friend"        , withFlag MsgIsLeagueFriend)
        ,("is_neighbor_of"          , withFlag MsgNeighbors)
        ,("is_origin_of_consort"    , withFlag MsgIsOriginOfConsort)
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
        ,("has_offensive_war"       , withFlag MsgHasOffensiveWar)
        ,("overlord_of"             , withFlag MsgOverlordOf)
        ,("is_owned_by"             , withFlag MsgIsOwnedBy)
        ,("preferred_emperor"       , withFlag MsgPreferredEmperor)
        ,("provinces_on_capital_continent_of" , withFlag MsgProvincesOnCapitalContinentOf)
        ,("release"                 , withFlag MsgReleaseVassal)
        ,("remove_claim"            , withFlag MsgRemoveClaim)
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
        [("change_culture"   , locAtomTagOrProvince (const MsgChangeCulture) MsgChangeSameCulture)
        -- above is province, below is country - use same messages for both
        ,("change_primary_culture", locAtomTagOrProvince (const MsgChangeCulture) MsgChangeSameCulture)
        ,("change_culture"   , locAtomTagOrProvince (const MsgChangeCulture) MsgChangeSameCulture)
        ,("change_religion"  , iconOrFlag MsgChangeReligion MsgChangeSameReligion Nothing)
        ,("consort_culture"  , locAtomTagOrProvince (const MsgConsortCultureIs) MsgConsortCultureIsSame)
        ,("continent"        , locAtomTagOrProvince (const MsgContinentIs) MsgContinentIsAs)
        ,("culture"          , locAtomTagOrProvince (const MsgCultureIs) MsgCultureIsAs)
        ,("culture_group"    , locAtomTagOrProvince (const MsgCultureIsGroup) MsgCultureGroupAs)
        ,("dominant_culture" , locAtomTagOrProvince (const MsgDominantCultureIs) MsgDominantCultureIsAs)
        ,("dominant_religion", locAtomTagOrProvince MsgDominantReligion MsgDominantReligionAs)
        ,("heir_culture"     , locAtomTagOrProvince (const MsgHeirCultureIs) MsgHeirCultureIsSame)
        ,("heir_nationality" , locAtomTagOrProvince (const MsgHeirNationality) MsgHeirNationalityAs)
        ,("heir_religion"    , locAtomTagOrProvince MsgHeirReligion MsgHeirReligionAs)
        ,("is_core"          , tagOrProvince MsgIsCoreOf MsgHasCoreOn (Just HOI4Country))
        ,("is_claim"         , tagOrProvince MsgHasClaim MsgHasClaimOn (Just HOI4Country))
        ,("is_harmonizing_with" , locAtomTagOrProvince MsgIsHarmonizingWith MsgIsHarmonizingWithProvince)
        ,("is_permanent_claim" , tagOrProvince MsgIsPermanentClaim MsgHasPermanentClaim (Just HOI4Country))
        ,("primary_culture"  , locAtomTagOrProvince (const MsgPrimaryCultureIs) MsgPrimaryCultureIsAs)
        ,("province_religion" , locAtomTagOrProvince MsgProvinceReligion MsgProvinceSameReligion)
        ,("religion"         , locAtomTagOrProvince MsgReligion MsgSameReligion)
        ,("religion_group"   , locAtomTagOrProvince MsgReligionGroup MsgSameReligionGroup)
        ,("ruler_culture"    , iconOrFlag MsgRulerCultureIs MsgRulerCultureIsSame Nothing)
        ,("ruler_religion"   , iconOrFlag MsgRulerReligionIs MsgRulerReligionIsSame Nothing)
        ,("set_consort_culture" , locAtomTagOrProvince (const MsgChangeConsortCulture) MsgChangeConsortSameCulture)
        ,("set_heir_culture" , locAtomTagOrProvince (const MsgChangeHeirCulture) MsgChangeHeirSameCulture)
        ,("set_heir_religion", locAtomTagOrProvince MsgSetHeirReligion MsgSetHeirReligionAs)
        ,("set_ruler_culture" , locAtomTagOrProvince (const MsgChangeRulerCulture) MsgChangeRulerSameCulture)
        ,("set_ruler_religion" , iconOrFlag MsgChangeRulerReligion MsgChangeRulerSameReligion Nothing)
        ,("trade_goods"      , locAtomTagOrProvince MsgProducesGoods MsgProducesSameGoods)
        ]

-- | Handlers for statements whose RHS may be either a tag or a province
handlersTagOrProvince :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersTagOrProvince = Tr.fromList
        [ -- obsolete
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
        ,("is_colonial_nation"          , withBool MsgIsColonialNation)
        ,("is_colony"                   , withBool MsgIsColony)
        ,("is_council_enabled"          , withBool MsgIsCouncilEnabled)
        ,("is_defender_of_faith"        , withBool MsgIsDefenderOfFaith)
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
        ,("is_random_new_world"         , withBool MsgRandomNewWorld)
        ,("is_reformation_center"       , withBool MsgIsCenterOfReformation)
        ,("is_religion_reformed"        , withBool MsgReligionReformed)
        ,("is_religious_center_province" , withBool MsgIsReligiousCenterProvince)
        ,("is_revolution_target"        , withBool MsgIsRevolutionTarget)
        ,("is_revolutionary"            , withBool MsgIsRevolutionary)
        ,("is_revolutionary_republic_trigger" , withBool MsgIsRevolutionaryRepublic)
        ,("is_sea"                      , withBool MsgIsSea) -- province or trade node
        ,("is_state"                    , withBool MsgIsState)
        ,("is_statists_in_power"        , withBool MsgIsStatistsInPower)
        ,("is_subject"                  , withBool MsgIsSubject)
        ,("is_territory"                , withBool MsgIsTerritory)
        ,("is_trade_league_leader"      , withBool MsgIsTradeLeagueLeader)
        ,("is_tribal"                   , withBool MsgIsTribal)
        ,("is_tutorial_active"          , withBool MsgIsInTutorial)
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
handlersNumProvinces :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
handlersNumProvinces = Tr.fromList
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
        ,("add_popularity"              , textValue "ideology" "popularity" MsgAddPopularity MsgAddPopularity tryLocAndIcon)
        ,("add_power_projection"        , textValue "type" "amount" MsgAddPowerProjection MsgAddPowerProjection tryLocAndIcon)
        ,("add_spy_network_from"        , textValue "who" "value" MsgAddSpyNetworkFrom MsgAddSpyNetworkFrom flagTextMaybe)
        ,("add_spy_network_in"          , textValue "who" "value" MsgAddSpyNetworkIn MsgAddSpyNetworkIn flagTextMaybe)
        ,("army_strength"               , textValue "who" "value" MsgArmyStrength MsgArmyStrength flagTextMaybe)
        ,("border_distance"             , textValue "who" "distance" MsgBorderDistance MsgBorderDistance flagTextMaybe)
        ,("estate_influence"            , textValue "estate" "influence" MsgEstateInfluence MsgEstateInfluence tryLocAndIcon)
        ,("estate_loyalty"              , textValue "estate" "loyalty" MsgEstateLoyalty MsgEstateLoyalty tryLocAndIcon)
        ,("estate_territory"            , textValue "estate" "territory" MsgEstateTerritory MsgEstateTerritory tryLocAndIcon)
        ,("had_country_flag"            , textValue "flag" "days" MsgHadCountryFlag MsgHadCountryFlag tryLocAndIcon)
        ,("had_global_flag"             , textValue "flag" "days" MsgHadGlobalFlag MsgHadGlobalFlag tryLocAndIcon)
        ,("had_heir_flag"               , textValue "flag" "days" MsgHadHeirFlag MsgHadHeirFlag tryLocAndIcon)
        ,("had_province_flag"           , textValue "flag" "days" MsgHadProvinceFlag MsgHadProvinceFlag tryLocAndIcon)
        ,("had_ruler_flag"              , textValue "flag" "days" MsgHadRulerFlag MsgHadRulerFlag tryLocAndIcon)
        ,("has_global_modifier_value"   , textValue "which" "value" MsgHasGlobalModifierValue MsgHasGlobalModifierValue tryLocAndLocMod)
        ,("has_spy_network_from"        , textValue "who" "value" MsgHasSpyNetworkFrom MsgHasSpyNetworkFrom flagTextMaybe)
        ,("has_spy_network_in"          , textValue "who" "value" MsgHasSpyNetworkIn MsgHasSpyNetworkIn flagTextMaybe)
        ,("has_won_war_against"         , textValue "who" "max_years_since" MsgHasWonWarAgainst MsgHasWonWarAgainst flagTextMaybe)
        ,("incident_variable_value"     , textValue "incident" "value" MsgIncidentVariableValue MsgIncidentVariableValue tryLocAndIconTitle)
        ,("institution_difference"      , textValue "who" "value" MsgInstitutionDifference MsgInstitutionDifference flagTextMaybe)
        ,("military_strength"           , textValue "who" "value" MsgMilitaryStrength MsgMilitaryStrength flagTextMaybe)
        ,("num_of_estate_privileges"    , textValue "estate" "value" MsgNumEstatePrivileges MsgNumEstatePrivileges tryLocAndIcon)
        ,("num_of_units_in_province"    , textValue "who" "amount" MsgNumUnitsInProvince MsgNumUnitsInProvince flagTextMaybe) -- TODO: Support type
        ,("num_investments_in_trade_company_region" , textValue "investment" "value" MsgNumInvestmentsInTradeCompanyReigion MsgNumInvestmentsInTradeCompanyReigion tryLocAndIcon)
        ,("naval_strength"              , textValue "who" "value" MsgNavalStrength MsgNavalStrength flagTextMaybe)
        ,("province_distance"           , textValue "who" "distance" MsgProvinceDistance MsgProvinceDistance flagTextMaybe)
        ,("school_opinion"              , textValue "who" "opinion" MsgSchoolOpinion MsgSchoolOpinion flagTextMaybe)
        ,("set_school_opinion"          , textValue "who" "opinion" MsgSetSchoolOpinion MsgSetSchoolOpinion flagTextMaybe)
        ,("set_victory_points"          , valueValue "province" "value" MsgSetVictoryPoints MsgSetVictoryPoints)
        ,("remove_building"             , textValue "type" "level" MsgRemoveBuilding MsgRemoveBuilding tryLocAndIcon)
        ,("remove_loot"                 , textValue "who" "amount" MsgRemoveLoot MsgRemoveLoot flagTextMaybe)
        ,("trade_goods_produced_amount" , textValue "trade_goods" "amount" MsgTradeGoodsProduced MsgTradeGoodsProduced tryLocAndIcon)
        ,("trading_part"                , textValue "trade_goods" "value" MsgTradingPart MsgTradingPart tryLocAndIcon)
        ,("trade_share"                 , textValue "country" "share" MsgTradeShare MsgTradeShare flagTextMaybe)
        ,("trust"                       , textValue "who" "value" MsgTrust MsgTrust flagTextMaybe)
        ,("war_score_against"           , textValue "who" "value" MsgWarscoreAgainst MsgWarscoreAgainst flagTextMaybe)
        ,("years_in_union_under"        , textValue "who" "years" MsgYearsInUnionUnder MsgYearsInUnionUnder flagTextMaybe)
        ,("modify_country_flag"         , withNonlocTextValue2 "flag" "value" MsgCountryFlag MsgModifyFlag) -- Localization/icon ignored
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
        ,("change_price"                 , changePrice)
        ,("create_admiral"               , createMilitaryLeader "admiral" True MsgCreateAdmiral MsgDefineAdmiral)
        ,("create_conquistador"          , createMilitaryLeader "conquistador" False MsgCreateConquistador MsgDefineConquistador)
        ,("create_explorer"              , createMilitaryLeader "explorer" True MsgCreateExplorer MsgDefineExplorer)
        ,("create_general"               , createMilitaryLeader "general" False MsgCreateGeneral MsgDefineGeneral)
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
        ,("has_completed_focus"          , hasFocus MsgHasCompletedFocus)
        ,("complete_national_focus"      , hasFocus MsgCompleteNationalFocus)
        ,("focus_progress"               , focusProgress MsgFocusProgress)
        ,("has_estate_led_regency"       , hasEstateLedRegency)
        ,("has_estate_influence_modifier", hasEstateModifier MsgEstateHasInfluenceModifier)
        ,("has_estate_loyalty_modifier"  , hasEstateModifier MsgEstateHasLoyaltyModifier)
        ,("has_great_project"            , hasGreatProject)
        ,("has_idea"                     , handleIdeas MsgHasIdea)
        ,("add_ideas"                    , handleIdeas MsgAddIdea)
        ,("remove_ideas"                 , handleIdeas MsgRemoveIdea)
        ,("add_timed_idea"               , handleTimedIdeas MsgAddTimedIdea)
        ,("modify_timed_idea"            , handleTimedIdeas MsgModifyTimedIdea)
        ,("swap_ideas"                    , handleSwapIdeas)
        ,("has_opinion"                  , hasOpinion MsgHasOpinion)
        ,("has_country_leader"           , hasCountryLeader)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion (\modid what who _years -> MsgAddOpinion modid what who))
        ,("has_reached_government_reform_tier" , hasGovernmentReforTier)
        ,("has_trade_company_investment_in_area", hasTradeCompanyInvestment)
        ,("is_in_war"                    , isInWar)
        ,("load_focus_tree"              , loadFocusTree)
        ,("news_event"                   , scope HOI4Country . triggerEvent MsgNewsEvent)
        ,("privateer_power"              , privateerPower)
        ,("province_event"               , scope HOI4ScopeState . triggerEvent MsgProvinceEvent)
        ,("region"                       , region)
        ,("remove_opinion_modifier"      , opinion MsgRemoveOpinionMod (\modid what who _years -> MsgRemoveOpinionMod modid what who))
--        ,("reverse_has_opinion"          , hasOpinion MsgReverseHasOpinion)
        ,("reverse_has_opinion_modifier" , opinion MsgReverseHasOpinionMod (\modid what who _years -> MsgReverseHasOpinionMod modid what who))
        ,("reverse_remove_opinion"       , opinion MsgReverseRemoveOpinionMod (\modid what who _years -> MsgReverseRemoveOpinionMod modid what who))
        ,("religion_years"               , religionYears)
        ,("set_ai_attitude"              , aiAttitude MsgSetAiAttitude)
        ,("set_autonomy"                 , setAutonomy)
        ,("set_country_flag"             , setCountryFlag)
        ,("has_country_flag"             , hasCountryFlag MsgCountryFlag)
        ,("has_global_flag"              , hasCountryFlag MsgGlobalFlag)
        ,("set_politics"                 , setPolitics)
        ,("set_party_name"               , setPartyName)
        ,("state_event"                  , scope HOI4ScopeState . triggerEvent MsgStateEvent)
        ,("reverse_add_casus_belli"      , addCB False)
        ,("trading_bonus"                , tradingBonus)
        ,("trading_policy_in_node"       , tradingPolicyInNode)
        ,("trigger_switch"               , triggerSwitch)
        ,("unit_leader_event"            , scope HOI4UnitLeader . triggerEvent MsgUnitLeaderEvent)
        ,("operative_leader_event"       , scope HOI4Operative . triggerEvent MsgOperativeEvent)

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
        ,("kill_advisor_by_category_effect" , killAdvisorByCategory)
        ,("select_primary_cult"             , simpleEffectAtom "cult" MsgSelectPrimaryCult)
        ,("set_great_project_tier_1"        , simpleEffectAtom "type" (MsgSetGreatProjectTier 1))
        ,("set_great_project_tier_2"        , simpleEffectAtom "type" (MsgSetGreatProjectTier 2))
        ,("set_great_project_tier_3"        , simpleEffectAtom "type" (MsgSetGreatProjectTier 3))
        ,("scaled_estate_land_share_reduce_effect" , simpleEffectAtom "estate" (MsgScaledEstateLandShareEffect False))
        ,("scaled_estate_land_share_add_effect" , simpleEffectAtom "estate" (MsgScaledEstateLandShareEffect True))
        ,("unlock_estate_privilege"         , simpleEffectAtom "estate_privilege" MsgUnlockEstatePrivilege)

        -- Variables
        ,("set_variable"                 , setVariable MsgSetVariable MsgSetVariableVal)
        ,("add_to_variable"              , setVariable MsgAddVariable MsgAddVariableVal)
        ,("subtract_variable"            , setVariable MsgSubVariable MsgSubVariableVal)
        ,("multiply_variable"            , setVariable MsgMulVariable MsgMulVariableVal)
        ,("divide_variable"              , setVariable MsgDivVariable MsgDivVariableVal)
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
        [("random", random)
        ,("random_list", randomList)
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
        ,("development_in_provinces" , numOwnedProvincesWith MsgDevelopmentInProvinces)
        ,("dominant_culture"    , dominantCulture)
        ,("dynasty"             , dynasty)
        ,("faction_in_power"    , factionInPower)
        ,("government_rank"     , govtRank)
        ,("has_casus_belli"     , taTypeFlag "type" "target" MsgHasCasusBelli)
        ,("has_dlc"             , hasDlc)
        ,("has_government_attribute" , hasGovermentAttribute)
        ,("has_heir"            , hasHeir)
        ,("has_leader_with"     , hasLeaderWith)
        ,("hre_reform_level"    , hreReformLevel)
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
        ,("set_government_rank" , setGovtRank)
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
        ,("tooltip"       , return $ return [])
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

-- | Predicate for matching text on boths sides
matchExactText :: Text -> Text -> GenericStatement -> Bool
matchExactText l r s@[pdx| $lhs = $rhs |] | l == lhs && r == T.toLower rhs = True
matchExactText _ _ _ = False
