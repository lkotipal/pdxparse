{-# LANGUAGE OverloadedStrings, ViewPatterns, MultiParamTypeClasses, ScopedTypeVariables #-}
module Messages (
        ScriptMessage (..)
    ,   template, templateDoc
    ,   message, messageText
    ,   imsg2doc, imsg2doc_html
    ,   IndentedMessage, IndentedMessages
    ,   module Doc
    ) where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Doc
import MessageTools
import SettingsTypes

-- dummy type required by the Shakespeare machinery
data Script = Script

data ScriptMessage
    = MsgUnprocessed {scriptMessageMsg :: Text}
    | MsgYes {scriptMessageIcon :: Text}
    | MsgNo {scriptMessageIcon :: Text}
    | MsgAddCardinal
    | MsgHeirDies
    | MsgRulerDies
    | MsgLoseCardinal
    | MsgGainADM {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainAT {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainAuth {scriptMessageAmt :: Double}
    | MsgGainBT {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainBP {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainBM {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainDIP {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainDoom {scriptMessageAmt :: Double}
    | MsgHeirGainClaim {scriptMessageAmt :: Double}
    | MsgGainDevotion {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainHordeUnity {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainImperialAuthority {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainKarma {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainLegitimacy {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainMIL {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainNavyTradition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainPapalInfluence {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainPrestige {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainStability {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainWarExhaustion {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainYearlyManpower {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainADMSkill {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainDIPSkill {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainMILSkill {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainSiegeProgress {scriptMessageAmt :: Double}
    | MsgGainPatAuth {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainPiety {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainRepTrad {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainInflation {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainLocalAutonomy {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgReformDesire {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainReformDesire {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainMercantilism {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainMP {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainMPFrac {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgSeparatism {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCountryMod
    | MsgProvMod
    | MsgPermanentProvMod
    | MsgRulerMod
    | MsgTradeMod
    | MsgAddMod {scriptMessageModid :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgGainMod {scriptMessageModid :: Text, scriptMessageType :: Text, scriptMessageName :: Text}
    | MsgGainModDur {scriptMessageModid :: Text, scriptMessageType :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgGainModPow {scriptMessageModid :: Text, scriptMessageType :: Text, scriptMessageName :: Text, scriptMessagePow :: Double}
    | MsgGainModPowDur {scriptMessageModid :: Text, scriptMessageType :: Text, scriptMessageName :: Text, scriptMessagePow :: Double, scriptMessageDays :: Double}
    | MsgActorGainsMod {scriptMessageModid :: Text, scriptMessageWho :: Text, scriptMessageType :: Text, scriptMessageName :: Text}
    | MsgActorGainsModDur {scriptMessageModid :: Text, scriptMessageWho :: Text, scriptMessageType :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgActorGainsModPow {scriptMessageModid :: Text, scriptMessageWho :: Text, scriptMessageType :: Text, scriptMessageName :: Text, scriptMessagePow :: Double}
    | MsgActorGainsModPowDur {scriptMessageModid :: Text, scriptMessageWho :: Text, scriptMessageType :: Text, scriptMessageName :: Text, scriptMessagePow :: Double, scriptMessageDays :: Double}
    | MsgHasModifier {scriptMessageModid :: Text, scriptMessageKind :: Text, scriptMessageName :: Text}
    | MsgRemoveModifier {scriptMessageModid :: Text, scriptMessageKind :: Text, scriptMessageName :: Text}
    | MsgAllOf
    | MsgOurCountry
    | MsgFROM
    | MsgPREV
    | MsgNoneOf
    | MsgAtLeastOneOf
    | MsgArea
    | MsgAnyActiveTradeNode
    | MsgAnyAlly
    | MsgAnyCoreCountry
    | MsgAnyCountry
    | MsgAnyEnemyCountry
    | MsgAnyKnownCountry
    | MsgAnyNeighborCountry
    | MsgAnyNeighborProvince
    | MsgAnyOwnedProvince
    | MsgAnyRival
    | MsgAnySubject
    | MsgCapital
    | MsgController
    | MsgEmperor
    | MsgAllCountries
    | MsgEveryCountry
    | MsgEveryEnemyCountry
    | MsgEveryKnownCountry
    | MsgEveryNeighborCountry
    | MsgEveryNeighborProvince
    | MsgEveryOwnedProvince
    | MsgEveryProvince
    | MsgEveryRival
    | MsgEverySubject
    | MsgHiddenEffect
    | MsgIf
    | MsgLimit
    | MsgOwner
    | MsgRandomActiveTradeNode
    | MsgRandomAlly
    | MsgRandomCoreCountry
    | MsgRandomCountry
    | MsgRandomKnownCountry
    | MsgRandomList
    | MsgRandomNeighborCountry
    | MsgRandomNeighborProvince
    | MsgRandomOwnedProvince
    | MsgRandomProvince
    | MsgRandomRival
    | MsgRandomChance {scriptMessageChance :: Double}
    | MsgRandom
    | MsgChangeGovernment {scriptMessageWhat :: Text}
    | MsgContinentIs {scriptMessageWhat :: Text}
    | MsgCultureIs {scriptMessageWhat :: Text}
    | MsgCultureIsGroup {scriptMessageWhat :: Text}
    | MsgRulerIsDynasty {scriptMessageWhat :: Text}
    | MsgDisasterEnds {scriptMessageWhat :: Text}
    | MsgGovernmentIs {scriptMessageWhat :: Text}
    | MsgGovernmentIsIcon {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgHasAdvisor {scriptMessageWhom :: Text}
    | MsgHasAdvisorType {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgHasTerrain {scriptMessageWhat :: Text}
    | MsgCavalrySpawnsCountry {scriptMessageWhom :: Text}
    | MsgCavalrySpawnsProvince {scriptMessageWhere :: Text}
    | MsgInfantrySpawnsCountry {scriptMessageWhom :: Text}
    | MsgInfantrySpawnsProvince {scriptMessageWhere :: Text}
    | MsgAdvisorDies {scriptMessageWho :: Text}
    | MsgPrimaryCultureIs {scriptMessageWhat :: Text}
    | MsgRegionIs {scriptMessageWhat :: Text}
    | MsgLoseAdvisor {scriptMessageWho :: Text}
    | MsgRemoveFromEstate {scriptMessageIcon :: Text, scriptMessageWhom :: Text}
    | MsgDisasterOngoing {scriptMessageWhat :: Text}
    | MsgProvinceIs {scriptMessageWhat :: Text}
    | MsgOwns {scriptMessageWhat :: Text}
    | MsgOwnsCore {scriptMessageWhat :: Text}
    | MsgControls {scriptMessageWhat :: Text}
    | MsgAdvisorExists {scriptMessageAdvisorID :: Double}
    | MsgAdvisorIsEmployed {scriptMessageAdvisorID :: Double}
    | MsgClearFlag {scriptMessageFlagType :: Text, scriptMessageName :: Text}
    | MsgHasFlag {scriptMessageFlagType :: Text, scriptMessageName :: Text}
    | MsgSetFlag {scriptMessageFlagType :: Text, scriptMessageName :: Text}
    | MsgHadFlag {scriptMessageCategory :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgCountryFlag
    | MsgProvinceFlag
    | MsgRulerFlag
    | MsgGlobalFlag
    | MsgHadCountryFlag {scriptMessage_icon :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgHadProvinceFlag {scriptMessage_icon :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgHadRulerFlag {scriptMessage_icon :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgHadGlobalFlag {scriptMessage_icon :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgColonySettlers {scriptMessageAmt :: Double}
    | MsgWasAtWar {scriptMessageAmt :: Double}
    | MsgHeirAge {scriptMessageAmt :: Double}
    | MsgYearIs {scriptMessageAmt :: Double}
    | MsgNumLoans {scriptMessageAmt :: Double}
    | MsgNumMercs {scriptMessageAmt :: Double}
    | MsgNumMissionaries {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumPorts {scriptMessageAmt :: Double}
    | MsgNumRebelArmies {scriptMessageAmt :: Double}
    | MsgNumEmbargoes {scriptMessageAmt :: Double}
    | MsgUnitsInProvince {scriptMessageAmt :: Double}
    | MsgNumCities {scriptMessageAmt :: Double}
    | MsgNumCitiesThan {scriptMessageWhom :: Text}
    | MsgToleranceToThis {scriptMessageAmt :: Double}
    | MsgRulerADM {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgADMTech {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgArmyTradition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyArmyTradition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBaseManpower {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBaseProduction {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBaseTax {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCreateAdmiral {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCreateConquistador {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCreateExplorer {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCreateGeneral {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDevelopment {scriptMessageIcon :: Text, scriptMessageDevelopment :: Double}
    | MsgRulerDIP {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDIPTech {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHordeUnity {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyHordeUnity {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgKarma {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLegitimacy {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyLegitimacy {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRulerMIL {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMILTech {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumAllies {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumCardinals {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumColonists {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumHeavyShips {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumLightShips {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumMerchants {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgStability {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTotalDevelopment {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTotalCardinals {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgUnrest {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMonthlyIncome {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgWarExhaustion {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMonthlyWarExhaustion {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgWarScore {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRepTrad {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyRepTrad {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgInflation {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLocalAutonomy {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgManpowerPercentage {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMercantilism {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgChangeGoods {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgCreateAdvisor {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgHasIdeaGroup {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgProducesGoods {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgEstateExists {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgHasEstate {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgAssignToEstate {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgRulerIsGeneral {scriptMessageIcon :: Text, scriptMessage_what :: Text}
    | MsgAlliedWith {scriptMessageWhom :: Text}
    | MsgCedeProvinceTo {scriptMessageWhom :: Text}
    | MsgControlledBy {scriptMessageWhom :: Text}
    | MsgDefensiveWarAgainst {scriptMessageWhom :: Text}
    | MsgDiscoverCountry {scriptMessageWhom :: Text}
    | MsgGainClaim {scriptMessageWho :: Text}
    | MsgGainPermanentClaimCountry {scriptMessageWho :: Text}
    | MsgGainPermanentClaimProvince {scriptMessageWhere :: Text}
    | MsgHasDiscovered {scriptMessageWhomOrWhere :: Text}
    | MsgDiscoveredBy {scriptMessageWhom :: Text}
    | MsgInherit {scriptMessageWhom :: Text}
    | MsgNeighbors {scriptMessageWhom :: Text}
    | MsgIsSubjectOf {scriptMessageWhom :: Text}
    | MsgLoseCoreCountry {scriptMessageWho :: Text}
    | MsgLoseCoreProvince {scriptMessageWhere :: Text}
    | MsgRoyalMarriageWith {scriptMessageWhom :: Text}
    | MsgOffensiveWarAgainst {scriptMessageWhom :: Text}
    | MsgOwnedBy {scriptMessageWhom :: Text}
    | MsgReleaseVassal {scriptMessageWhom :: Text}
    | MsgUnderSiegeBy {scriptMessageWhom :: Text}
    | MsgCountryIs {scriptMessageWho :: Text}
    | MsgTruceWith {scriptMessageWhom :: Text}
    | MsgAtWarWith {scriptMessageWhom :: Text}
    | MsgMakeWhitePeace {scriptMessageWhom :: Text}
    | MsgCountryExists {scriptMessageWho :: Text}
    | MsgExists {scriptMessageYn :: Bool}
    | MsgSameReligion {scriptMessageWhom :: Text}
    | MsgReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgSameReligionGroup {scriptMessageWhom :: Text}
    | MsgReligionGroup {scriptMessage_ :: Text, scriptMessageWhat :: Text}
    | MsgChangeSameReligion {scriptMessageWhom :: Text}
    | MsgChangeReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgIsCoreOf {scriptMessageWhom :: Text}
    | MsgHasCoreOn {scriptMessageWhat :: Text}
    | MsgHasClaim {scriptMessageWho :: Text}
    | MsgHasClaimOn {scriptMessageWhat :: Text}
    | MsgIsAIControlled {scriptMessageYn :: Bool}
    | MsgHasCardinal {scriptMessageYn :: Bool}
    | MsgHasHeir {scriptMessageYn :: Bool}
    | MsgHasOwnerCulture {scriptMessageYn :: Bool}
    | MsgHasOwnerReligion {scriptMessageYn :: Bool}
    | MsgHasPort {scriptMessageYn :: Bool}
    | MsgHasSeatInParliament {scriptMessageYn :: Bool}
    | MsgIsInRegency {scriptMessageYn :: Bool}
    | MsgUnderSiege {scriptMessageYn :: Bool}
    | MsgAtWar {scriptMessageYn :: Bool}
    | MsgIsCapital {scriptMessageYn :: Bool}
    | MsgIsCity {scriptMessageYn :: Bool}
    | MsgIsColony {scriptMessageYn :: Bool}
    | MsgIsEmperor {scriptMessageYn :: Bool}
    | MsgIsFemale {scriptMessageYn :: Bool}
    | MsgIsLesserInUnion {scriptMessageYn :: Bool}
    | MsgIsLooted {scriptMessageYn :: Bool}
    | MsgIsOverseas {scriptMessageYn :: Bool}
    | MsgIsPartOfHRE {scriptMessageYn :: Bool}
    | MsgIsCenterOfReformation {scriptMessageYn :: Bool}
    | MsgIsSubject {scriptMessageYn :: Bool}
    | MsgPapacyIsActive {scriptMessageYn :: Bool}
    | MsgHasBeenPlayer {scriptMessageYn :: Bool}
    | MsgGainCB {scriptMessageCbtype :: Text, scriptMessageWhom :: Text}
    | MsgGainCBDuration {scriptMessageCbtype :: Text, scriptMessageWhom :: Text, scriptMessageMonths :: Double}
    | MsgReverseGainCB {scriptMessageCbtype :: Text, scriptMessageWho :: Text}
    | MsgReverseGainCBDuration {scriptMessageCbtype :: Text, scriptMessageWho :: Text, scriptMessageMonths :: Double}
    | MsgFactionGainInfluence {scriptMessageIcon :: Text, scriptMessageWhom :: Text, scriptMessageAmt :: Double}
    | MsgFactionInPower {scriptMessageIcon :: Text, scriptMessageWhom :: Text}
    | MsgHasFactions {scriptMessageYn :: Bool}
    | MsgHasBuilding {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgIndefinitely
    | MsgForDays {scriptMessageDays :: Double}
    | MsgEstateLoyalty {scriptMessageIcon :: Text, scriptMessageWho :: Text, scriptMessageAmt :: Double}
    | MsgEstateInfluence {scriptMessageIcon :: Text, scriptMessageWho :: Text, scriptMessageAmt :: Double}
    | MsgAddEstateLoyalty {scriptMessageIcon :: Text, scriptMessageWho :: Text, scriptMessageAmt :: Double}
    | MsgAddEstateInfluence {scriptMessageIcon :: Text, scriptMessageWho :: Text, scriptMessageAmt :: Double}
    | MsgEstateLoyaltyModifier {scriptMessageIcon :: Text, scriptMessageWho :: Text, scriptMessageWhat :: Text, scriptMessageAmt :: Double, scriptMessageDur :: Text}
    | MsgEstateInfluenceModifier {scriptMessageIcon :: Text, scriptMessageWho :: Text, scriptMessageWhat :: Text, scriptMessageAmt :: Double, scriptMessageDur :: Text}
    | MsgAddOpinion {scriptMessageWhat :: Text, scriptMessageWhom :: Text}
    | MsgReverseAddOpinion {scriptMessageWhat :: Text, scriptMessageWho :: Text}
    | MsgAddOpinionDur {scriptMessageWhat :: Text, scriptMessageWhom :: Text, scriptMessageYears :: Double}
    | MsgReverseAddOpinionDur {scriptMessageWhat :: Text, scriptMessageWho :: Text, scriptMessageYears :: Double}
    | MsgHasOpinionMod {scriptMessageWhat :: Text, scriptMessageWhom :: Text}
    | MsgRemoveOpinionMod {scriptMessageWhat :: Text, scriptMessageWhom :: Text}
    | MsgAddTreasury {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgAddYearsOfIncome {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNewHeir
    | MsgNewHeirClaim {scriptMessageClaim :: Double}
    | MsgNewHeirDynasty {scriptMessageFlag :: Text}
    | MsgNewHeirDynastyClaim {scriptMessageFlag :: Text, scriptMessageClaim :: Double}
    | MsgNewHeirAge {scriptMessageAge :: Double}
    | MsgNewHeirAgeClaim {scriptMessageAge :: Double, scriptMessageClaim :: Double}
    | MsgNewHeirAgeFlag {scriptMessageAge :: Double, scriptMessageFlag :: Text}
    | MsgNewHeirAgeFlagClaim {scriptMessageAge :: Double, scriptMessageFlag :: Text, scriptMessageClaim :: Double}
    | MsgBuildToForcelimitLand {scriptMessageInficon :: Text, scriptMessageInfantry :: Double, scriptMessageCavicon :: Text, scriptMessageCavalry :: Double, scriptMessageArticon :: Text, scriptMessageArtillery :: Double}
    | MsgBuildToForcelimitNavy {scriptMessageHeavyicon :: Text, scriptMessageHeavy :: Double, scriptMessageLighticon :: Text, scriptMessageLight :: Double, scriptMessageGallicon :: Text, scriptMessageGalley :: Double, scriptMessageTranspicon :: Text, scriptMessageTransport :: Double}
    | MsgProvinceEvent
    | MsgCountryEvent
    | MsgTriggerEvent {scriptMessageEvttype :: Text, scriptMessageEvtid :: Text, scriptMessageName :: Text}
    | MsgTriggerEventDays {scriptMessageEvttype :: Text, scriptMessageEvtid :: Text, scriptMessageName :: Text, scriptMessageDays :: Double}
    | MsgDeclareWarWithCB {scriptMessageWhom :: Text, scriptMessageCb :: Text}
    | MsgGainAdvisor {scriptMessageSkill :: Double}
    | MsgGainAdvisorDiscount {scriptMessageSkill :: Double}
    | MsgGainAdvisorLoc {scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorLocDiscount {scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorName {scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorNameDiscount {scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorNameLoc {scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorNameLocDiscount {scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorType {scriptMessageAdvtype :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeDiscount {scriptMessageAdvtype :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeLoc {scriptMessageAdvtype :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeLocDiscount {scriptMessageAdvtype :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeName {scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeNameDiscount {scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeNameLoc {scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainAdvisorTypeNameLocDiscount {scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisor {scriptMessageFemale :: Bool, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorDiscount {scriptMessageFemale :: Bool, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorLoc {scriptMessageFemale :: Bool, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorLocDiscount {scriptMessageFemale :: Bool, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorName {scriptMessageFemale :: Bool, scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorNameDiscount {scriptMessageFemale :: Bool, scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorNameLoc {scriptMessageFemale :: Bool, scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorNameLocDiscount {scriptMessageFemale :: Bool, scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorType {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeDiscount {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeLoc {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeLocDiscount {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeName {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeNameDiscount {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeNameLoc {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgGainFemaleAdvisorTypeNameLocDiscount {scriptMessageFemale :: Bool, scriptMessageAdvtype :: Text, scriptMessageName :: Text, scriptMessageWhere :: Text, scriptMessageSkill :: Double}
    | MsgRebelLeaderRuler
    | MsgNewRuler {scriptMessageRegent :: Bool}
    | MsgNewRulerLeader {scriptMessageRegent :: Bool, scriptMessageName :: Text}
    | MsgNewRulerAttribs {scriptMessageRegent :: Bool}
    | MsgNewRulerLeaderAttribs {scriptMessageRegent :: Bool, scriptMessageName :: Text}
    | MsgLeaderRuler {scriptMessageRegent :: Bool, scriptMessageName :: Text}
    | MsgNewRulerName {scriptMessageName :: Text}
    | MsgNewRulerDynasty {scriptMessageName :: Text}
    | MsgNewRulerAge {scriptMessageAmt :: Double}
    | MsgNewRulerAdm {scriptMessageFixed :: Bool, scriptMessageAmt :: Double}
    | MsgNewRulerDip {scriptMessageFixed :: Bool, scriptMessageAmt :: Double}
    | MsgNewRulerMil {scriptMessageFixed :: Bool, scriptMessageAmt :: Double}
    | MsgNewRulerClaim {scriptMessageAmt :: Double}
    | MsgNewRulerFixed {scriptMessageAdm :: Double, scriptMessageDip :: Double, scriptMessageMil :: Double}
    | MsgEstateHasInfluenceModifier {scriptMessageIcon :: Text, scriptMessageEstate :: Text, scriptMessageModifier :: Text}
    | MsgTriggerSwitch
    | MsgTriggerSwitchClause {scriptMessageCond :: Text}
    | MsgProvinceHasRebels {scriptMessageIcon :: Text, scriptMessageRtype :: Text}
    | MsgRebelsFriendlyTo {scriptMessageFriend :: Text}
    | MsgRebelsLedBy {scriptMessageLeader :: Text}
    | MsgRebelsGainProgress {scriptMessageAmt :: Double}
    | MsgSpawnRebels {scriptMessageRtype :: Text, scriptMessageSize :: Double, scriptMessageFriend :: Text, scriptMessageLeader :: Text, scriptMessageWin :: Bool, scriptMessageProgress :: Text}
    | MsgRebelsHaveRisen {scriptMessageIcon :: Text, scriptMessageRtype :: Text}
    | MsgTagGainsCore {scriptMessageWho :: Text}
    | MsgGainCoreOnProvince {scriptMessageProv :: Text}
    | MsgHasDLC {scriptMessageIcon :: Text, scriptMessageDlc :: Text}
    | MsgProvince {scriptMessageWhere :: Text}
    | MsgTechGroup {scriptMessageIcon :: Text, scriptMessageName :: Text}
    | MsgNumOfReligion {scriptMessageIcon :: Text, scriptMessageName :: Text, scriptMessageAmt :: Double}
    | MsgStrongestTradePower {scriptMessageWho :: Text}
    | MsgAreaIs {scriptMessageWhat :: Text}
    | MsgDominantReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgHREReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgSetHREReligionLocked {scriptMessageYn :: Bool}
    | MsgSetHREReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgSetHREHereticReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgSignWestphalia
    | MsgHRELeaguesEnabled {scriptMessageYn :: Bool}
    | MsgHREReligionLocked {scriptMessageYn :: Bool}
    | MsgHREWestphalia {scriptMessageYn :: Bool}
    | MsgIsElector {scriptMessageYn :: Bool}
    | MsgNoHREReforms
    | MsgHREPassedReform {scriptMessageWhat :: Text}
    | MsgEnableHRELeagues
    | MsgIsInLeagueWar {scriptMessageYn :: Bool}
    | MsgIsLeagueEnemy {scriptMessageWhom :: Text}
    | MsgReligionYears {scriptMessageIcon :: Text, scriptMessageName :: Text, scriptMessageYears :: Double}
    | MsgHasIdea {scriptMessageWhat :: Text}
    | MsgReligionProvinces {scriptMessageIcon :: Text, scriptMessageName :: Text, scriptMessageAmt :: Double}
    | MsgGoodsProvinces {scriptMessageIcon :: Text, scriptMessageName :: Text, scriptMessageAmt :: Double}
    | MsgHasAristocraticIdea {scriptMessageName :: Text, scriptMessageNum :: Int}
    | MsgHasEconomicIdea {scriptMessageName :: Text, scriptMessageNum :: Int}
    | MsgHasDefensiveIdea {scriptMessageName :: Text, scriptMessageNum :: Int}
    | MsgHasInnovativeIdea {scriptMessageName :: Text, scriptMessageNum :: Int}
    | MsgHasOffensiveIdea {scriptMessageName :: Text, scriptMessageNum :: Int}
    | MsgHasMaritimeIdea {scriptMessageName :: Text, scriptMessageNum :: Int}
    | MsgColonists {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMayExplore
    | MsgGainColonialRange {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalSettlers {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalTariffs {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavalForcelimitMod {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainOverseasCB
    | MsgGainPrimitivesCB
    | MsgNavyTradition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyNavyTradition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHeavyShipCombatAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLightShipCombatAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGalleyCombatAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalShipRepair {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalShipCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRegimentCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavalLeaderManeuver {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBlockadeEfficiency {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainSeaRepair
    | MsgPrimitives {scriptMessageYn :: Bool}
    | MsgGlobalTaxModifier {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBuildCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyInflationReduction {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgInterestPerAnnum {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalAutonomy {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLandMaintenanceMod {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavalMaintenanceMod {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgProdEff {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDevelCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainReligiousCB
    | MsgMissionaries {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgStabilityCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMissionaryStrength {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgToleranceHeathen {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgToleranceHeretic {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgToleranceTrue {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyPapalInfluence {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyDevotion {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMonthlyFervor {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgChurchPowerModifier {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgPrestige {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearlyPrestige {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMissionaryStrengthVsHeretics {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCultureConvCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasOpinion {scriptMessageAmt :: Double, scriptMessageWhom :: Text}
    | MsgNormalOrHistoricalNations {scriptMessageYn :: Bool}
    | MsgIsCustomNation {scriptMessageYn :: Bool}
    | MsgReligionEnabled {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgCapitalIs {scriptMessageWhat :: Text}
    | MsgFullIdeaGroup {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgTradeIncomePercentage {scriptMessageAmt :: Double}
    | MsgReligiousUnity {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgReligiousUnityBonus {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasADM {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasDIP {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasMIL {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRankDuchy
    | MsgRankKingdom
    | MsgRankEmpire
    | MsgSetRankDuchy
    | MsgSetRankKingdom
    | MsgSetRankEmpire
    | MsgOverextension {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRandomNewWorld {scriptMessageYn :: Bool}
    | MsgIsColonialNation {scriptMessageYn :: Bool}
    | MsgIsFormerColonialNation {scriptMessageYn :: Bool}
    | MsgIsNomad {scriptMessageYn :: Bool}
    | MsgReligionReformed {scriptMessageYn :: Bool}
    | MsgChangeTag {scriptMessageWho :: Text}
    | MsgSetInEmpire {scriptMessageYn :: Bool}
    | MsgHasSecondaryReligion {scriptMessageYn :: Bool}
    | MsgSecondaryReligion {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgIsDefenderOfFaith {scriptMessageYn :: Bool}
    | MsgLegitimacyOrHordeUnity {scriptMessageAmt :: Double}
    | MsgCheckVariable {scriptMessage_icon :: Text, scriptMessageWhat :: Text, scriptMessageAmt :: Double}
    | MsgChangeTechGroup {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgChangeUnitType {scriptMessageIcon :: Text, scriptMessageWhat :: Text}
    | MsgNoBaseWeight
    | MsgAIBaseWeight {scriptMessageAmt :: Double}
    | MsgAIFactorOneline {scriptMessageFactor :: Text, scriptMessageMultiplier :: Double}
    | MsgAIFactorHeader {scriptMessageMultiplier :: Double}
    | MsgLucky {scriptMessageYn :: Bool}
    | MsgHasArtistLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasDiplomatLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasArmyReformerLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasNaturalScientistLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasNavyReformerLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasTheologianLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasTraderLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgHasStatesmanLevel {scriptMessageIcon :: Text, scriptMessageLevel :: Double}
    | MsgNumRoyalMarriages {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgIsBankrupt {scriptMessageYn :: Bool}
    | MsgNumColonialSubjects {scriptMessageAmt :: Double}
    | MsgTradeEfficiency {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTradeEfficiencyBonus {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasWarTaxes {scriptMessageYn :: Bool}
    | MsgRevoltPercentage {scriptMessageAmt :: Double}
    | MsgHasAnyDisaster {scriptMessageYn :: Bool}
    | MsgHasActivePolicy {scriptMessageWhat :: Text}
    | MsgHasDucats {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasParliament {scriptMessageYn :: Bool}
    | MsgHasTruce {scriptMessageYn :: Bool}
    | MsgNumRebelControlledProvinces {scriptMessageAmt :: Double}
    | MsgFortLevel {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasTradeModifier {scriptMessage_icon :: Text, scriptMessageWho :: Text, scriptMessageWhat :: Text}
    | MsgIsMonth {scriptMessageWhat :: Text}
    | MsgIsSea {scriptMessageYn :: Bool}
    | MsgHeavyShip {scriptMessageWhom :: Text}
    | MsgLightShip {scriptMessageWhom :: Text}
    | MsgGalley {scriptMessageWhom :: Text}
    | MsgNumColonies {scriptMessageAmt :: Double}
    | MsgChangeCulture {scriptMessageWhat :: Text}
    | MsgNavalForcelimit {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBlockade {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCreateAlliance {scriptMessageWhom :: Text}
    | MsgAddLocalUnrest {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGoldIncomePercentage {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgIsTribal {scriptMessageYn :: Bool}
    | MsgSetCapital {scriptMessageWhat :: Text}
    | MsgChangePrimaryCulture {scriptMessageWhat :: Text}
    | MsgColonialRegion {scriptMessageWhere :: Text}
    | MsgJuniorUnionWith {scriptMessageWhom :: Text}
    | MsgSeniorUnionWith {scriptMessageWhom :: Text}
    | MsgVassalOf {scriptMessageWhom :: Text}
    | MsgOverlordOf {scriptMessageWhom :: Text}
    | MsgChangeProvinceName {scriptMessageWhat :: Text}
    | MsgRenameCapital {scriptMessageWhat :: Text}
    | MsgOwnsOrVassal {scriptMessageWhere :: Text}
    | MsgIsInColonialRange {scriptMessageWhom :: Text}
    | MsgConstructingGreatProject {scriptMessageWhat :: Text}
    | MsgConstructing {scriptMessageWhat :: Text}
    | MsgStartConstructingGreatProject {scriptMessageWhat :: Text}
    | MsgCancelConstruction
    | MsgYearsOfIncome {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLibertyDesire {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainLibertyDesire {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgColonialParent
    | MsgAlways {scriptMessageYn :: Bool}
    | MsgCapitalCultureDominant
    | MsgNumUnions {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNumVassals {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgFreeVassal {scriptMessageWhom :: Text}
    | MsgHasMissionary {scriptMessageYn :: Bool}
    | MsgNavyPercentage {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgWasForceConverted {scriptMessageYn :: Bool}
    | MsgFemaleEmperorAllowed {scriptMessageYn :: Bool}
    | MsgImperialAuthority {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHasFemaleHeir {scriptMessageYn :: Bool}
    | MsgPiety {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgIsInTutorial {scriptMessageYn :: Bool}
    | MsgGainFervor {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGainChurchPower {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgIncomeFromVassals {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTimeToFabricateClaims {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDiploAnnexCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgAEImpact {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDiploRep {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgEnvoyTravelTime {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDiploRelations {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgVassalForcelimitContribution {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgUnjustifiedDemands {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgInfantryCombatAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCavalryCombatAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgArtilleryCombatAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgShipDurability {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMoraleOfArmies {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMoraleOfNavies {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavalAttrition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDiscipline {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNationalManpowerMod {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgManpowerRecoverySpeed {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgAvailableMercs {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGarrisonSize {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLandAttrition {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLandForcelimitMod {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgPrestigeDecay {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMercCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTechCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgPossibleAdvisors {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgReduceInflationCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLeadersWithoutUpkeep {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgAdvisorCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCoreCreationCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMercMaintenance {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgADMTechCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDIPTechCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGoodsProducedMod {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavalLeaderFire {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavalLeaderShock {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgPrestigeFromLand {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgPrestigeFromNaval {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgDiplomats {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgWarExhaustionCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgImproveRelations {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgProvinceWarscoreCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgReducedStabImpacts
    | MsgNationalUnrest {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgYearsOfSeparatism {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgAccCultureThreshold {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgBetterRelationsOverTime {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgIdeaCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMerchants {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRecruitmentTime {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgShipbuildingTime {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalTradePower {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgEmbargoEff {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgPrivateerEff {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgGlobalSpyDefence {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMaySabotageReputation
    | MsgSpyOffense {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCovertActionRelationImpact {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMayStudyTech
    | MsgMaySowDiscontent
    | MsgMayAgitateForLiberty
    | MsgMayInfiltrateAdministration
    | MsgRebelSupportEff {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgArmyTraditionDecay {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgNavyTraditionDecay {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgInfantryCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCavalryCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgMILTechCost {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgHostileCoreCreation {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgCaravanPower {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLandLeaderFire {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLandLeaderShock {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLandLeaderManeuver {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgLeaderSiege {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgFortDefense {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgFortMaintenance {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgReinforceSpeed {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgAttritionForEnemies {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgSiegeAbility {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgRecoverArmyMoraleSpeed {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTradeRange {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTradeSteering {scriptMessageIcon :: Text, scriptMessageAmt :: Double}
    | MsgTextIs {scriptMessageWhat :: Text}
    | MsgAnyOwnedPlanet
    | MsgAnyOwnedShip
    | MsgAnyPop
    | MsgEveryOwnedPlanet
    | MsgEveryOwnedPop
    | MsgRandomOwnedShip
    | MsgRandomPop
    | MsgRandomSystem
    | MsgRandomTile
    | MsgGainTrait {scriptMessageIcon :: Text, scriptMessageWhat :: Text}

useEnglish :: [Text] -> Bool
useEnglish [] = True
useEnglish ("en":_) = True

instance RenderMessage Script ScriptMessage where
    renderMessage _ (useEnglish -> True) msg = case msg of
        MsgUnprocessed {scriptMessageMsg = _msg}
            -> _msg
        MsgYes {scriptMessageIcon = _icon}
            -> mconcat
                [ _icon
                , " Yes"
                ]
        MsgNo {scriptMessageIcon = _icon}
            -> mconcat
                [ _icon
                , " No"
                ]
        MsgAddCardinal
            -> "Gain a cardinal"
        MsgHeirDies
            -> "Heir dies"
        MsgRulerDies
            -> "Ruler dies"
        MsgLoseCardinal
            -> "Lose a cardinal"
        MsgGainADM {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " administrative power"
                ]
        MsgGainAT {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " army tradition"
                ]
        MsgGainAuth {scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , toMessage (colourNum True _amt)
                , " authority"
                ]
        MsgGainBT {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " base tax"
                ]
        MsgGainBP {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " base production"
                ]
        MsgGainBM {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " base manpower"
                ]
        MsgGainDIP {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " diplomatic power"
                ]
        MsgGainDoom {scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , toMessage (colourNum False _amt)
                , " doom"
                ]
        MsgHeirGainClaim {scriptMessageAmt = _amt}
            -> mconcat
                [ "Heir "
                , gainsOrLoses _amt
                , " "
                , toMessage (colourNum True _amt)
                , " claim strength"
                ]
        MsgGainDevotion {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " devotion"
                ]
        MsgGainHordeUnity {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " horde unity"
                ]
        MsgGainImperialAuthority {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " imperial authority"
                ]
        MsgGainKarma {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " karma"
                ]
        MsgGainLegitimacy {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " legitimacy"
                ]
        MsgGainMIL {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " military power"
                ]
        MsgGainNavyTradition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " navy tradition"
                ]
        MsgGainPapalInfluence {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " papal influence"
                ]
        MsgGainPrestige {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " prestige"
                ]
        MsgGainStability {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " stability"
                ]
        MsgGainWarExhaustion {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum False _amt)
                , " war exhaustion"
                ]
        MsgGainYearlyManpower {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " "
                , plural _amt "year's" "years'"
                , " worth of manpower"
                ]
        MsgGainADMSkill {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Ruler "
                , gainsOrLoses _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " administrative skill"
                ]
        MsgGainDIPSkill {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Ruler "
                , gainsOrLoses _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " diplomatic skill"
                ]
        MsgGainMILSkill {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Ruler "
                , gainsOrLoses _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " military skill"
                ]
        MsgGainSiegeProgress {scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , toMessage (colourNum True _amt)
                , " siege progress"
                ]
        MsgGainPatAuth {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (reducedNum (colourPc True) _amt)
                , " Patriarch authority"
                ]
        MsgGainPiety {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (reducedNum (colourPc True) _amt)
                , " Piety"
                ]
        MsgGainRepTrad {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (reducedNum (colourNum True) _amt)
                , " republican tradition"
                ]
        MsgGainInflation {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourPc False _amt)
                , " inflation"
                ]
        MsgGainLocalAutonomy {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourPc False _amt)
                , " local autonomy"
                ]
        MsgReformDesire {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "{{icon|catholic}} Catholicism has at least "
                , _icon
                , " "
                , toMessage (reducedNum plainPc _amt)
                , " reform desire"
                ]
        MsgGainReformDesire {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "{{icon|catholic}} Catholicism "
                , gainsOrLoses _amt
                , " "
                , _icon
                , " "
                , toMessage (reducedNum (colourPc False) _amt)
                , " reform desire"
                ]
        MsgGainMercantilism {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (reducedNum (colourPc True) _amt)
                , " mercantilism"
                ]
        MsgGainMP {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " manpower"
                ]
        MsgGainMPFrac {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " manpower equal to "
                , toMessage (reducedNum (colourPc True) _amt)
                , " of maximum"
                ]
        MsgSeparatism {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "year" "years"
                , " of separatism"
                ]
        MsgCountryMod
            -> "country modifier"
        MsgProvMod
            -> "province modifier"
        MsgPermanentProvMod
            -> "permanent province modifier"
        MsgRulerMod
            -> "ruler modifier"
        MsgTradeMod
            -> "trade modifier"
        MsgAddMod {scriptMessageModid = _modid, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Gain modifier "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> for "
                , toMessage (roundNumNoSpace _days)
                , " days"
                ]
        MsgGainMod {scriptMessageModid = _modid, scriptMessageType = _type, scriptMessageName = _name}
            -> mconcat
                [ "Gain "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " -->"
                ]
        MsgGainModDur {scriptMessageModid = _modid, scriptMessageType = _type, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Gain "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> for "
                , toMessage (roundNumNoSpace _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgGainModPow {scriptMessageModid = _modid, scriptMessageType = _type, scriptMessageName = _name, scriptMessagePow = _pow}
            -> mconcat
                [ "Gain "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> ("
                , toMessage (colourNumSign True _pow)
                , " Power"
                ]
        MsgGainModPowDur {scriptMessageModid = _modid, scriptMessageType = _type, scriptMessageName = _name, scriptMessagePow = _pow, scriptMessageDays = _days}
            -> mconcat
                [ "Gain "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> ("
                , toMessage (colourNumSign True _pow)
                , " Power) for "
                , toMessage (plainNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgActorGainsMod {scriptMessageModid = _modid, scriptMessageWho = _who, scriptMessageType = _type, scriptMessageName = _name}
            -> mconcat
                [ _who
                , " gains "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " -->"
                ]
        MsgActorGainsModDur {scriptMessageModid = _modid, scriptMessageWho = _who, scriptMessageType = _type, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ _who
                , " gains "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> for "
                , toMessage (plainNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgActorGainsModPow {scriptMessageModid = _modid, scriptMessageWho = _who, scriptMessageType = _type, scriptMessageName = _name, scriptMessagePow = _pow}
            -> mconcat
                [ _who
                , " gains "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> ("
                , toMessage (colourNumSign True _pow)
                , " Power"
                ]
        MsgActorGainsModPowDur {scriptMessageModid = _modid, scriptMessageWho = _who, scriptMessageType = _type, scriptMessageName = _name, scriptMessagePow = _pow, scriptMessageDays = _days}
            -> mconcat
                [ _who
                , " gains "
                , _type
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " --> ("
                , toMessage (colourNumSign True _pow)
                , " Power) for "
                , toMessage (plainNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgHasModifier {scriptMessageModid = _modid, scriptMessageKind = _kind, scriptMessageName = _name}
            -> mconcat
                [ "Has "
                , _kind
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _modid
                , " -->"
                ]
        MsgRemoveModifier {scriptMessageModid = _modid, scriptMessageKind = _kind, scriptMessageName = _name}
            -> mconcat
                [ "Remove "
                , _kind
                , " ''\""
                , _name
                , "\"'' <!-- "
                , _modid
                , " -->"
                ]
        MsgAllOf
            -> "All of:"
        MsgOurCountry
            -> "Our country:"
        MsgFROM
            -> "FROM:"
        MsgPREV
            -> "PREV:"
        MsgNoneOf
            -> "None of:"
        MsgAtLeastOneOf
            -> "At least one of:"
        MsgArea
            -> "Area containing this province:"
        MsgAnyActiveTradeNode
            -> "Any trade node with a merchant present:"
        MsgAnyAlly
            -> "Any ally:"
        MsgAnyCoreCountry
            -> "Any country with a core on this province:"
        MsgAnyCountry
            -> "Any country in the world:"
        MsgAnyEnemyCountry
            -> "Any enemy country:"
        MsgAnyKnownCountry
            -> "Any known country:"
        MsgAnyNeighborCountry
            -> "Any neighbouring country:"
        MsgAnyNeighborProvince
            -> "Any neighbouring province:"
        MsgAnyOwnedProvince
            -> "Any owned province:"
        MsgAnyRival
            -> "Any rival:"
        MsgAnySubject
            -> "Any subject:"
        MsgCapital
            -> "Capital:"
        MsgController
            -> "Province controller:"
        MsgEmperor
            -> "The Holy Roman Emperor:"
        MsgAllCountries
            -> "All countries in the world:"
        MsgEveryCountry
            -> "Every country in the world:"
        MsgEveryEnemyCountry
            -> "Every enemy country:"
        MsgEveryKnownCountry
            -> "Every known country:"
        MsgEveryNeighborCountry
            -> "Every neighbouring country:"
        MsgEveryNeighborProvince
            -> "Every neighbouring province:"
        MsgEveryOwnedProvince
            -> "Every owned province:"
        MsgEveryProvince
            -> "Every province in the world:"
        MsgEveryRival
            -> "Every rival:"
        MsgEverySubject
            -> "Every subject country:"
        MsgHiddenEffect
            -> "Hidden effect:"
        MsgIf
            -> "If:"
        MsgLimit
            -> "Limited to:"
        MsgOwner
            -> "Province owner:"
        MsgRandomActiveTradeNode
            -> "One random trade node with a merchant present:"
        MsgRandomAlly
            -> "One random ally:"
        MsgRandomCoreCountry
            -> "One random country with a core on this province:"
        MsgRandomCountry
            -> "One random country:"
        MsgRandomKnownCountry
            -> "One random known country:"
        MsgRandomList
            -> "One of the following at random:"
        MsgRandomNeighborCountry
            -> "One random neighbouring country:"
        MsgRandomNeighborProvince
            -> "One random neighbouring province:"
        MsgRandomOwnedProvince
            -> "One random owned province:"
        MsgRandomProvince
            -> "One random province:"
        MsgRandomRival
            -> "One random rival:"
        MsgRandomChance {scriptMessageChance = _chance}
            -> mconcat
                [ toMessage (plainPc _chance)
                , " chance of:"
                ]
        MsgRandom
            -> "One of the following at random:"
        MsgChangeGovernment {scriptMessageWhat = _what}
            -> mconcat
                [ "Change government to "
                , _what
                ]
        MsgContinentIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Continent is "
                , _what
                ]
        MsgCultureIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Culture is "
                , _what
                ]
        MsgCultureIsGroup {scriptMessageWhat = _what}
            -> mconcat
                [ "Culture is in "
                , _what
                , " group"
                ]
        MsgRulerIsDynasty {scriptMessageWhat = _what}
            -> mconcat
                [ "Ruler is of "
                , _what
                , " dynasty"
                ]
        MsgDisasterEnds {scriptMessageWhat = _what}
            -> mconcat
                [ "Disaster "
                , toMessage (iquotes _what)
                , " ends"
                ]
        MsgGovernmentIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Government is "
                , _what
                ]
        MsgGovernmentIsIcon {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Government is "
                , _icon
                , " "
                , _what
                ]
        MsgHasAdvisor {scriptMessageWhom = _whom}
            -> mconcat
                [ "Has advisor "
                , _whom
                ]
        MsgHasAdvisorType {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Has "
                , _icon
                , " "
                , _what
                , " advisor"
                ]
        MsgHasTerrain {scriptMessageWhat = _what}
            -> mconcat
                [ "Has "
                , _what
                , " terrain"
                ]
        MsgCavalrySpawnsCountry {scriptMessageWhom = _whom}
            -> mconcat
                [ "A cavalry regiment loyal to "
                , _whom
                , " spawns"
                ]
        MsgCavalrySpawnsProvince {scriptMessageWhere = _where}
            -> mconcat
                [ "A cavalry regiment spawns in "
                , _where
                ]
        MsgInfantrySpawnsCountry {scriptMessageWhom = _whom}
            -> mconcat
                [ "An infantry regiment loyal to "
                , _whom
                , " spawns"
                ]
        MsgInfantrySpawnsProvince {scriptMessageWhere = _where}
            -> mconcat
                [ "An infantry regiment spawns in "
                , _where
                ]
        MsgAdvisorDies {scriptMessageWho = _who}
            -> mconcat
                [ "Advisor "
                , _who
                , " dies"
                ]
        MsgPrimaryCultureIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Primary culture is "
                , _what
                ]
        MsgRegionIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Province is in "
                , _what
                , " region"
                ]
        MsgLoseAdvisor {scriptMessageWho = _who}
            -> mconcat
                [ "Advisor "
                , _who
                , " leaves the country's court"
                ]
        MsgRemoveFromEstate {scriptMessageIcon = _icon, scriptMessageWhom = _whom}
            -> mconcat
                [ "Remove province from the "
                , _icon
                , " "
                , _whom
                , " estate"
                ]
        MsgDisasterOngoing {scriptMessageWhat = _what}
            -> mconcat
                [ "The "
                , toMessage (iquotes _what)
                , " disaster is ongoing"
                ]
        MsgProvinceIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Province is "
                , _what
                ]
        MsgOwns {scriptMessageWhat = _what}
            -> mconcat
                [ "Owns province "
                , _what
                ]
        MsgOwnsCore {scriptMessageWhat = _what}
            -> mconcat
                [ "Owns core province "
                , _what
                ]
        MsgControls {scriptMessageWhat = _what}
            -> mconcat
                [ "Controls province "
                , _what
                ]
        MsgAdvisorExists {scriptMessageAdvisorID = _advisorID}
            -> mconcat
                [ "Advisor with ID "
                , toMessage (plainNum _advisorID)
                , " exists"
                ]
        MsgAdvisorIsEmployed {scriptMessageAdvisorID = _advisorID}
            -> mconcat
                [ "Advisor with ID "
                , toMessage (plainNum _advisorID)
                , " is employed"
                ]
        MsgClearFlag {scriptMessageFlagType = _flagType, scriptMessageName = _name}
            -> mconcat
                [ "Clear "
                , _flagType
                , " flag <tt>"
                , _name
                , "</tt>"
                ]
        MsgHasFlag {scriptMessageFlagType = _flagType, scriptMessageName = _name}
            -> mconcat
                [ toMessage (T.toTitle _flagType)
                , " flag <tt>"
                , _name
                , "</tt> is set"
                ]
        MsgSetFlag {scriptMessageFlagType = _flagType, scriptMessageName = _name}
            -> mconcat
                [ "Set "
                , _flagType
                , " flag <tt>"
                , _name
                , "</tt>"
                ]
        MsgHadFlag {scriptMessageCategory = _category, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Has had "
                , _category
                , " flag "
                , _name
                , " for "
                , toMessage (roundNum _days)
                , " days"
                ]
        MsgCountryFlag
            -> "country"
        MsgProvinceFlag
            -> "province"
        MsgRulerFlag
            -> "ruler"
        MsgGlobalFlag
            -> "global"
        MsgHadCountryFlag {scriptMessage_icon = __icon, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Has had country flag <tt>"
                , _name
                , "</tt> for at least "
                , toMessage (roundNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgHadProvinceFlag {scriptMessage_icon = __icon, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Has had province flag <tt>"
                , _name
                , "</tt> for at least "
                , toMessage (roundNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgHadRulerFlag {scriptMessage_icon = __icon, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Has had ruler flag <tt>"
                , _name
                , "</tt> for at least "
                , toMessage (roundNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgHadGlobalFlag {scriptMessage_icon = __icon, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Global flag <tt>"
                , _name
                , "</tt> has been set for at least "
                , toMessage (roundNum _days)
                , " "
                , plural _days "day" "days"
                ]
        MsgColonySettlers {scriptMessageAmt = _amt}
            -> mconcat
                [ "Colony has at least "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "settler" "settlers"
                ]
        MsgWasAtWar {scriptMessageAmt = _amt}
            -> mconcat
                [ "Was at war within the last "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "month" "months"
                ]
        MsgHeirAge {scriptMessageAmt = _amt}
            -> mconcat
                [ "Heir is at least "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "year" "years"
                , " old"
                ]
        MsgYearIs {scriptMessageAmt = _amt}
            -> mconcat
                [ "Year is "
                , toMessage (roundNumNoSpace _amt)
                , " or later"
                ]
        MsgNumLoans {scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , toMessage (plainNum _amt)
                , " "
                , plural _amt "loan" "loans"
                ]
        MsgNumMercs {scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , toMessage (roundNum _amt)
                , " mercenary "
                , plural _amt "regiment" "regiments"
                ]
        MsgNumMissionaries {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "missionary" "missionaries"
                ]
        MsgNumPorts {scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , toMessage (plainNum _amt)
                , " "
                , plural _amt "port" "ports"
                ]
        MsgNumRebelArmies {scriptMessageAmt = _amt}
            -> mconcat
                [ "At least "
                , toMessage (roundNum _amt)
                , " rebel "
                , plural (round _amt) "army is" "armies are"
                , " present in the country"
                ]
        MsgNumEmbargoes {scriptMessageAmt = _amt}
            -> mconcat
                [ "Is embargoing at least "
                , toMessage (roundNum _amt)
                , " other "
                , plural (round _amt) "country" "countries"
                ]
        MsgUnitsInProvince {scriptMessageAmt = _amt}
            -> mconcat
                [ "Province contains at least "
                , toMessage (roundNum _amt)
                , " "
                , plural (round _amt) "regiment" "regiments"
                ]
        MsgNumCities {scriptMessageAmt = _amt}
            -> mconcat
                [ "Owns at least "
                , toMessage (roundNum _amt)
                , " non-colony "
                , plural _amt "province" "provinces"
                ]
        MsgNumCitiesThan {scriptMessageWhom = _whom}
            -> mconcat
                [ "Owns at least as many cities as "
                , _whom
                ]
        MsgToleranceToThis {scriptMessageAmt = _amt}
            -> mconcat
                [ "Tolerance to this religion is at least "
                , toMessage (colourNum True _amt)
                ]
        MsgRulerADM {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Ruler's "
                , _icon
                , " administrative skill is at least "
                , toMessage (roundNum _amt)
                ]
        MsgADMTech {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Administrative technology is at least "
                , toMessage (roundNum _amt)
                ]
        MsgArmyTradition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Army tradition is at least "
                , toMessage (plainNum _amt)
                ]
        MsgYearlyArmyTradition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Yearly army tradition"
                ]
        MsgBaseManpower {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Base manpower is at least "
                , toMessage (roundNum _amt)
                ]
        MsgBaseProduction {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Base production is at least "
                , toMessage (roundNum _amt)
                ]
        MsgBaseTax {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Base tax is at least "
                , toMessage (roundNum _amt)
                ]
        MsgCreateAdmiral {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Gain "
                , _icon
                , " admiral with "
                , toMessage (roundNum _amt)
                , " navy tradition"
                ]
        MsgCreateConquistador {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Gain "
                , _icon
                , " conquistador with "
                , toMessage (roundNum _amt)
                , " navy tradition"
                ]
        MsgCreateExplorer {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Gain "
                , _icon
                , " explorer with "
                , toMessage (roundNum _amt)
                , " navy tradition"
                ]
        MsgCreateGeneral {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Gain "
                , _icon
                , " general with "
                , toMessage (roundNum _amt)
                , " army tradition"
                ]
        MsgDevelopment {scriptMessageIcon = _icon, scriptMessageDevelopment = _development}
            -> mconcat
                [ _icon
                , " Development is at least "
                , toMessage (roundNum _development)
                ]
        MsgRulerDIP {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Ruler's "
                , _icon
                , " diplomatic skill is at least "
                , toMessage (roundNum _amt)
                ]
        MsgDIPTech {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Diplomatic technology is at least "
                , toMessage (roundNum _amt)
                ]
        MsgHordeUnity {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Horde unity is at least "
                , toMessage (roundNum _amt)
                ]
        MsgYearlyHordeUnity {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Yearly horde unity"
                ]
        MsgKarma {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Karma is at least "
                , toMessage (colourPc True _amt)
                ]
        MsgLegitimacy {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Legitimacy is at least "
                , toMessage (roundPc _amt)
                ]
        MsgYearlyLegitimacy {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourPcSign True _amt)
                , " Yearly legitimacy"
                ]
        MsgRulerMIL {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Ruler's "
                , _icon
                , " military skill is at least "
                , toMessage (roundNum _amt)
                ]
        MsgMILTech {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Military technology is at least "
                , toMessage (roundNum _amt)
                ]
        MsgNumAllies {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "ally" "allies"
                ]
        MsgNumCardinals {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "cardinal" "cardinals"
                ]
        MsgNumColonists {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "colonist" "colonists"
                ]
        MsgNumHeavyShips {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " heavy "
                , plural _amt "ship" "ships"
                ]
        MsgNumLightShips {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " light "
                , plural _amt "ship" "ships"
                ]
        MsgNumMerchants {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "merchant" "merchants"
                ]
        MsgStability {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Stability is at least "
                , toMessage (colourNumSign True _amt)
                ]
        MsgTotalDevelopment {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Country's total "
                , _icon
                , " development is at least "
                , toMessage (roundNum _amt)
                ]
        MsgTotalCardinals {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "At least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "cardinal" "cardinals"
                , " exist in the world"
                ]
        MsgUnrest {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Unrest is at least "
                , _icon
                , " "
                , toMessage (colourNum False _amt)
                ]
        MsgMonthlyIncome {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Monthly income is at least "
                , toMessage (colourNum True _amt)
                , " ducats"
                ]
        MsgWarExhaustion {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " War exhaustion is at least "
                , toMessage (colourNum False _amt)
                ]
        MsgMonthlyWarExhaustion {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNum False _amt)
                , " Monthly war exhaustion"
                ]
        MsgWarScore {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "War score is at least "
                , _icon
                , " "
                , toMessage (colourPc True _amt)
                ]
        MsgRepTrad {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Republican tradition is at least "
                , toMessage (reducedNum roundPc _amt)
                ]
        MsgYearlyRepTrad {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Yearly republican tradition"
                ]
        MsgInflation {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Inflation is at least "
                , toMessage (colourPc False _amt)
                ]
        MsgLocalAutonomy {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Local autonomy is at least "
                , toMessage (colourPc False _amt)
                ]
        MsgManpowerPercentage {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Manpower reserves are at least "
                , toMessage (reducedNum plainPc _amt)
                , " of maximum"
                ]
        MsgMercantilism {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Mercantilism is at least "
                , toMessage (reducedNum plainPc _amt)
                ]
        MsgChangeGoods {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Change province's goods produced to "
                , _icon
                , " "
                , _what
                ]
        MsgCreateAdvisor {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Gain "
                , _icon
                , " "
                , _what
                , " advisor"
                ]
        MsgHasIdeaGroup {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Has activated "
                , _icon
                , " "
                , _what
                ]
        MsgProducesGoods {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Produces "
                , _icon
                , " "
                , _what
                ]
        MsgEstateExists {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ _icon
                , " "
                , _what
                , " estate exists"
                ]
        MsgHasEstate {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Has estate "
                , _icon
                , " "
                , _what
                ]
        MsgAssignToEstate {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Assign province to the "
                , _icon
                , " "
                , _what
                , " estate"
                ]
        MsgRulerIsGeneral {scriptMessageIcon = _icon, scriptMessage_what = __what}
            -> mconcat
                [ "Ruler is a "
                , _icon
                , " general"
                ]
        MsgAlliedWith {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is allied to "
                , _whom
                ]
        MsgCedeProvinceTo {scriptMessageWhom = _whom}
            -> mconcat
                [ "Cede province to "
                , _whom
                ]
        MsgControlledBy {scriptMessageWhom = _whom}
            -> mconcat
                [ "Province is controlled by "
                , _whom
                ]
        MsgDefensiveWarAgainst {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is defending in a war against "
                , _whom
                ]
        MsgDiscoverCountry {scriptMessageWhom = _whom}
            -> mconcat
                [ "Discover "
                , _whom
                ]
        MsgGainClaim {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " gains a claim on this province"
                ]
        MsgGainPermanentClaimCountry {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " gains a permanent claim on this province"
                ]
        MsgGainPermanentClaimProvince {scriptMessageWhere = _where}
            -> mconcat
                [ "Gain a permanent claim on "
                , _where
                ]
        MsgHasDiscovered {scriptMessageWhomOrWhere = _whomOrWhere}
            -> mconcat
                [ "Has discovered "
                , _whomOrWhere
                ]
        MsgDiscoveredBy {scriptMessageWhom = _whom}
            -> mconcat
                [ "Has been discovered by "
                , _whom
                ]
        MsgInherit {scriptMessageWhom = _whom}
            -> mconcat
                [ "Inherit "
                , _whom
                ]
        MsgNeighbors {scriptMessageWhom = _whom}
            -> mconcat
                [ "Has a border with "
                , _whom
                ]
        MsgIsSubjectOf {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is a subject of "
                , _whom
                ]
        MsgLoseCoreCountry {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " loses their core on this province"
                ]
        MsgLoseCoreProvince {scriptMessageWhere = _where}
            -> mconcat
                [ "Lose core on "
                , _where
                ]
        MsgRoyalMarriageWith {scriptMessageWhom = _whom}
            -> mconcat
                [ "Has a royal marriage with "
                , _whom
                ]
        MsgOffensiveWarAgainst {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is attacking in a war against "
                , _whom
                ]
        MsgOwnedBy {scriptMessageWhom = _whom}
            -> mconcat
                [ "Province is owned by "
                , _whom
                ]
        MsgReleaseVassal {scriptMessageWhom = _whom}
            -> mconcat
                [ "Release "
                , _whom
                , " as a vassal"
                ]
        MsgUnderSiegeBy {scriptMessageWhom = _whom}
            -> mconcat
                [ "Province is under siege by "
                , _whom
                ]
        MsgCountryIs {scriptMessageWho = _who}
            -> mconcat
                [ "Is "
                , _who
                ]
        MsgTruceWith {scriptMessageWhom = _whom}
            -> mconcat
                [ "Has a truce with "
                , _whom
                ]
        MsgAtWarWith {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is at war with "
                , _whom
                ]
        MsgMakeWhitePeace {scriptMessageWhom = _whom}
            -> mconcat
                [ "Make a white peace with "
                , _whom
                ]
        MsgCountryExists {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " exists"
                ]
        MsgExists {scriptMessageYn = _yn}
            -> toMessage (ifThenElseT _yn "Exists" "Does ''not'' exist")
        MsgSameReligion {scriptMessageWhom = _whom}
            -> mconcat
                [ "Religion is same as "
                , _whom
                ]
        MsgReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Religion is "
                , _icon
                , " "
                , _what
                ]
        MsgSameReligionGroup {scriptMessageWhom = _whom}
            -> mconcat
                [ "Religion group is same as "
                , _whom
                ]
        MsgReligionGroup {scriptMessage_ = __, scriptMessageWhat = _what}
            -> mconcat
                [ "Religion is in "
                , _what
                , " group"
                ]
        MsgChangeSameReligion {scriptMessageWhom = _whom}
            -> mconcat
                [ "Change religion to that of "
                , _whom
                ]
        MsgChangeReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Change religion to "
                , _icon
                , " "
                , _what
                ]
        MsgIsCoreOf {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is a core of "
                , _whom
                ]
        MsgHasCoreOn {scriptMessageWhat = _what}
            -> mconcat
                [ "Has a core on "
                , _what
                ]
        MsgHasClaim {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " has a claim on this province"
                ]
        MsgHasClaimOn {scriptMessageWhat = _what}
            -> mconcat
                [ "Has a claim on "
                , _what
                ]
        MsgIsAIControlled {scriptMessageYn = _yn}
            -> mconcat
                [ "Is "
                , toMessage (ifThenElseT _yn "AI" "player")
                , "-controlled"
                ]
        MsgHasCardinal {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " a cardinal"
                ]
        MsgHasHeir {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " an heir"
                ]
        MsgHasOwnerCulture {scriptMessageYn = _yn}
            -> mconcat
                [ "Has "
                , toMessage (ifThenElseT _yn "the same culture as" "a different culture from")
                , " its owner"
                ]
        MsgHasOwnerReligion {scriptMessageYn = _yn}
            -> mconcat
                [ "Has "
                , toMessage (ifThenElseT _yn "the same religion as" "a different religion from")
                , " its owner"
                ]
        MsgHasPort {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " a port"
                ]
        MsgHasSeatInParliament {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " a seat in Parliament"
                ]
        MsgIsInRegency {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " in a regency"
                ]
        MsgUnderSiege {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " under siege"
                ]
        MsgAtWar {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " at war"
                ]
        MsgIsCapital {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " the country's capital"
                ]
        MsgIsCity {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a city"
                ]
        MsgIsColony {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a colony"
                ]
        MsgIsEmperor {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " the Holy Roman Emperor"
                ]
        MsgIsFemale {scriptMessageYn = _yn}
            -> mconcat
                [ "Is "
                , toMessage (ifThenElseT _yn "female" "male")
                ]
        MsgIsLesserInUnion {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a lesser partner in a personal union"
                ]
        MsgIsLooted {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " looted"
                ]
        MsgIsOverseas {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " overseas"
                ]
        MsgIsPartOfHRE {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " part of the Holy Roman Empire"
                ]
        MsgIsCenterOfReformation {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a Center of Reformation"
                ]
        MsgIsSubject {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a subject nation"
                ]
        MsgPapacyIsActive {scriptMessageYn = _yn}
            -> mconcat
                [ "Papal interaction is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " active"
                ]
        MsgHasBeenPlayer {scriptMessageYn = _yn}
            -> mconcat
                [ "Has"
                , toMessage (ifThenElseT _yn "" " ''never''")
                , " been player-controlled"
                ]
        MsgGainCB {scriptMessageCbtype = _cbtype, scriptMessageWhom = _whom}
            -> mconcat
                [ "Gain "
                , _cbtype
                , " casus belli against "
                , _whom
                ]
        MsgGainCBDuration {scriptMessageCbtype = _cbtype, scriptMessageWhom = _whom, scriptMessageMonths = _months}
            -> mconcat
                [ "Gain "
                , _cbtype
                , " casus belli against "
                , _whom
                , " for "
                , toMessage (roundNum _months)
                , " months"
                ]
        MsgReverseGainCB {scriptMessageCbtype = _cbtype, scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " gains "
                , _cbtype
                , " casus belli against this country"
                ]
        MsgReverseGainCBDuration {scriptMessageCbtype = _cbtype, scriptMessageWho = _who, scriptMessageMonths = _months}
            -> mconcat
                [ _who
                , " gains "
                , _cbtype
                , " casus belli against this country for "
                , toMessage (roundNum _months)
                , " months"
                ]
        MsgFactionGainInfluence {scriptMessageIcon = _icon, scriptMessageWhom = _whom, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (plainNum _amt)
                , " influence for the "
                , _whom
                , " faction"
                ]
        MsgFactionInPower {scriptMessageIcon = _icon, scriptMessageWhom = _whom}
            -> mconcat
                [ _icon
                , " "
                , _whom
                , " is in power"
                ]
        MsgHasFactions {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " factions"
                ]
        MsgHasBuilding {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Has "
                , _icon
                , " "
                , _what
                , " building"
                ]
        MsgIndefinitely
            -> "indefinitely"
        MsgForDays {scriptMessageDays = _days}
            -> mconcat
                [ "for "
                , toMessage (roundNum _days)
                , " days"
                ]
        MsgEstateLoyalty {scriptMessageIcon = _icon, scriptMessageWho = _who, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , _who
                , " has at least "
                , toMessage (plainNum _amt)
                , " loyalty"
                ]
        MsgEstateInfluence {scriptMessageIcon = _icon, scriptMessageWho = _who, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , _who
                , " has at least "
                , toMessage (plainNum _amt)
                , " influence"
                ]
        MsgAddEstateLoyalty {scriptMessageIcon = _icon, scriptMessageWho = _who, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , _who
                , " "
                , gainsOrLoses _amt
                , " "
                , toMessage (colourNum True _amt)
                , " loyalty"
                ]
        MsgAddEstateInfluence {scriptMessageIcon = _icon, scriptMessageWho = _who, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , _who
                , " "
                , gainsOrLoses _amt
                , " "
                , toMessage (plainNum _amt)
                , " influence"
                ]
        MsgEstateLoyaltyModifier {scriptMessageIcon = _icon, scriptMessageWho = _who, scriptMessageWhat = _what, scriptMessageAmt = _amt, scriptMessageDur = _dur}
            -> mconcat
                [ _icon
                , " "
                , _who
                , " gains loyalty modifier "
                , toMessage (iquotes _what)
                , " ("
                , toMessage (colourNum True _amt)
                , " loyalty "
                , _dur
                ]
        MsgEstateInfluenceModifier {scriptMessageIcon = _icon, scriptMessageWho = _who, scriptMessageWhat = _what, scriptMessageAmt = _amt, scriptMessageDur = _dur}
            -> mconcat
                [ _icon
                , " "
                , _who
                , " gains influence modifier "
                , toMessage (iquotes _what)
                , " ("
                , toMessage (colourNum True _amt)
                , " influence) "
                , _dur
                ]
        MsgAddOpinion {scriptMessageWhat = _what, scriptMessageWhom = _whom}
            -> mconcat
                [ "Gain opinion modifier "
                , toMessage (iquotes _what)
                , " towards "
                , _whom
                ]
        MsgReverseAddOpinion {scriptMessageWhat = _what, scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " gains opinion modifier "
                , toMessage (iquotes _what)
                , " towards this country"
                ]
        MsgAddOpinionDur {scriptMessageWhat = _what, scriptMessageWhom = _whom, scriptMessageYears = _years}
            -> mconcat
                [ "Gain opinion modifier "
                , toMessage (iquotes _what)
                , " towards "
                , _whom
                , " for "
                , toMessage (plainNum _years)
                , " years"
                ]
        MsgReverseAddOpinionDur {scriptMessageWhat = _what, scriptMessageWho = _who, scriptMessageYears = _years}
            -> mconcat
                [ _who
                , " gains opinion modifier "
                , toMessage (iquotes _what)
                , " towards this country for "
                , toMessage (plainNum _years)
                , " years"
                ]
        MsgHasOpinionMod {scriptMessageWhat = _what, scriptMessageWhom = _whom}
            -> mconcat
                [ "Has opinion modifier "
                , toMessage (iquotes _what)
                , " towards "
                , _whom
                ]
        MsgRemoveOpinionMod {scriptMessageWhat = _what, scriptMessageWhom = _whom}
            -> mconcat
                [ "Lose opinion modifier "
                , toMessage (iquotes _what)
                , " towards "
                , _whom
                ]
        MsgAddTreasury {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " ducats"
                ]
        MsgAddYearsOfIncome {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " ducats equal to "
                , toMessage (colourNum True _amt)
                , " "
                , plural (round _amt) "year" "years"
                , " of income"
                ]
        MsgNewHeir
            -> "Gain a new heir"
        MsgNewHeirClaim {scriptMessageClaim = _claim}
            -> mconcat
                [ "Gain a new heir with claim strength "
                , toMessage (plainNum _claim)
                ]
        MsgNewHeirDynasty {scriptMessageFlag = _flag}
            -> mconcat
                [ "Gain a new heir of the same dynasty as "
                , _flag
                ]
        MsgNewHeirDynastyClaim {scriptMessageFlag = _flag, scriptMessageClaim = _claim}
            -> mconcat
                [ "Gain a new heir with claim strength "
                , toMessage (roundNum _claim)
                ]
        MsgNewHeirAge {scriptMessageAge = _age}
            -> mconcat
                [ "Gain a new "
                , toMessage (roundNum _age)
                , " year old heir"
                ]
        MsgNewHeirAgeClaim {scriptMessageAge = _age, scriptMessageClaim = _claim}
            -> mconcat
                [ "Gain a new "
                , toMessage (roundNum _age)
                , " year old heir with claim strength "
                , toMessage (roundNum _claim)
                ]
        MsgNewHeirAgeFlag {scriptMessageAge = _age, scriptMessageFlag = _flag}
            -> mconcat
                [ "Gain a new "
                , toMessage (roundNum _age)
                , " year old heir of the same dynasty as "
                , _flag
                ]
        MsgNewHeirAgeFlagClaim {scriptMessageAge = _age, scriptMessageFlag = _flag, scriptMessageClaim = _claim}
            -> mconcat
                [ "Gain a new "
                , toMessage (roundNum _age)
                , " year old heir of the same dynasty as "
                , _flag
                , " with claim strength "
                , toMessage (roundNum _claim)
                ]
        MsgBuildToForcelimitLand {scriptMessageInficon = _inficon, scriptMessageInfantry = _infantry, scriptMessageCavicon = _cavicon, scriptMessageCavalry = _cavalry, scriptMessageArticon = _articon, scriptMessageArtillery = _artillery}
            -> mconcat
                [ "Build land units to forcelimit: "
                , _inficon
                , " "
                , toMessage (reducedNum plainPc _infantry)
                , " infantry, "
                , _cavicon
                , " "
                , toMessage (reducedNum plainPc _cavalry)
                , " cavalry, "
                , _articon
                , " "
                , toMessage (reducedNum plainPc _artillery)
                , " artillery"
                ]
        MsgBuildToForcelimitNavy {scriptMessageHeavyicon = _heavyicon, scriptMessageHeavy = _heavy, scriptMessageLighticon = _lighticon, scriptMessageLight = _light, scriptMessageGallicon = _gallicon, scriptMessageGalley = _galley, scriptMessageTranspicon = _transpicon, scriptMessageTransport = _transport}
            -> mconcat
                [ "Build naval units to forcelimit: "
                , _heavyicon
                , " "
                , toMessage (reducedNum plainPc _heavy)
                , " heavy ships, "
                , _lighticon
                , " "
                , toMessage (reducedNum plainPc _light)
                , " light ships, "
                , _gallicon
                , " "
                , toMessage (reducedNum plainPc _galley)
                , " galleys, "
                , _transpicon
                , " "
                , toMessage (reducedNum plainPc _transport)
                , " transports"
                ]
        MsgProvinceEvent
            -> "province event"
        MsgCountryEvent
            -> "country event"
        MsgTriggerEvent {scriptMessageEvttype = _evttype, scriptMessageEvtid = _evtid, scriptMessageName = _name}
            -> mconcat
                [ "Trigger "
                , _evttype
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _evtid
                , " -->"
                ]
        MsgTriggerEventDays {scriptMessageEvttype = _evttype, scriptMessageEvtid = _evtid, scriptMessageName = _name, scriptMessageDays = _days}
            -> mconcat
                [ "Trigger "
                , _evttype
                , " "
                , toMessage (iquotes _name)
                , " <!-- "
                , _evtid
                , " --> in "
                , toMessage (roundNum _days)
                , " days"
                ]
        MsgDeclareWarWithCB {scriptMessageWhom = _whom, scriptMessageCb = _cbtype}
            -> mconcat
                [ "Declare war on "
                , _whom
                , " using "
                , _cbtype
                , " casus belli"
                ]
        MsgGainAdvisor {scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor"
                ]
        MsgGainAdvisorDiscount {scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor (50% cheaper to employ)"
                ]
        MsgGainAdvisorLoc {scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor in "
                , _where
                ]
        MsgGainAdvisorLocDiscount {scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainAdvisorName {scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor named "
                , _name
                ]
        MsgGainAdvisorNameDiscount {scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor named "
                , _name
                , " (50% cheaper to employ)"
                ]
        MsgGainAdvisorNameLoc {scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor named "
                , _name
                , " in "
                , _where
                ]
        MsgGainAdvisorNameLocDiscount {scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " advisor named "
                , _name
                , " in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainAdvisorType {scriptMessageAdvtype = _advtype, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor"
                ]
        MsgGainAdvisorTypeDiscount {scriptMessageAdvtype = _advtype, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor (50% cheaper to employ)"
                ]
        MsgGainAdvisorTypeLoc {scriptMessageAdvtype = _advtype, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor in "
                , _where
                ]
        MsgGainAdvisorTypeLocDiscount {scriptMessageAdvtype = _advtype, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainAdvisorTypeName {scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor named "
                , _name
                ]
        MsgGainAdvisorTypeNameDiscount {scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor named "
                , _name
                , " (50% cheaper to employ)"
                ]
        MsgGainAdvisorTypeNameLoc {scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor named "
                , _name
                , " in "
                , _where
                ]
        MsgGainAdvisorTypeNameLocDiscount {scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , _advtype
                , " advisor named "
                , _name
                , " in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisor {scriptMessageFemale = _female, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor"
                ]
        MsgGainFemaleAdvisorDiscount {scriptMessageFemale = _female, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorLoc {scriptMessageFemale = _female, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor in "
                , _where
                ]
        MsgGainFemaleAdvisorLocDiscount {scriptMessageFemale = _female, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorName {scriptMessageFemale = _female, scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor named "
                , _name
                ]
        MsgGainFemaleAdvisorNameDiscount {scriptMessageFemale = _female, scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor named "
                , _name
                , " (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorNameLoc {scriptMessageFemale = _female, scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor named "
                , _name
                , " in "
                , _where
                ]
        MsgGainFemaleAdvisorNameLocDiscount {scriptMessageFemale = _female, scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " advisor named "
                , _name
                , " in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorType {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor"
                ]
        MsgGainFemaleAdvisorTypeDiscount {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorTypeLoc {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor in "
                , _where
                ]
        MsgGainFemaleAdvisorTypeLocDiscount {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorTypeName {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor named "
                , _name
                ]
        MsgGainFemaleAdvisorTypeNameDiscount {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor named "
                , _name
                , " (50% cheaper to employ)"
                ]
        MsgGainFemaleAdvisorTypeNameLoc {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor named "
                , _name
                , " in "
                , _where
                ]
        MsgGainFemaleAdvisorTypeNameLocDiscount {scriptMessageFemale = _female, scriptMessageAdvtype = _advtype, scriptMessageName = _name, scriptMessageWhere = _where, scriptMessageSkill = _skill}
            -> mconcat
                [ "Gain skill "
                , toMessage (roundNum _skill)
                , " "
                , toMessage (ifThenElseT _female "female" "male")
                , " "
                , _advtype
                , " advisor named "
                , _name
                , " in "
                , _where
                , " (50% cheaper to employ)"
                ]
        MsgRebelLeaderRuler
            -> "The leader of the rebels becomes the country's new ruler"
        MsgNewRuler {scriptMessageRegent = _regent}
            -> mconcat
                [ "A new "
                , toMessage (ifThenElseT _regent "regent" "ruler")
                , " comes to power"
                ]
        MsgNewRulerLeader {scriptMessageRegent = _regent, scriptMessageName = _name}
            -> mconcat
                [ "The leader "
                , _name
                , " becomes "
                , toMessage (ifThenElseT _regent "regent" "ruler")
                ]
        MsgNewRulerAttribs {scriptMessageRegent = _regent}
            -> mconcat
                [ "A new "
                , toMessage (ifThenElseT _regent "regent" "ruler")
                , " comes to power with the following attributes:"
                ]
        MsgNewRulerLeaderAttribs {scriptMessageRegent = _regent, scriptMessageName = _name}
            -> mconcat
                [ "The leader "
                , _name
                , " becomes "
                , toMessage (ifThenElseT _regent "regent" "ruler")
                , " with the following attributes:"
                ]
        MsgLeaderRuler {scriptMessageRegent = _regent, scriptMessageName = _name}
            -> mconcat
                [ "The leader "
                , _name
                , " comes to power as "
                , toMessage (ifThenElseT _regent "regent" "ruler")
                ]
        MsgNewRulerName {scriptMessageName = _name}
            -> mconcat
                [ "Named "
                , _name
                ]
        MsgNewRulerDynasty {scriptMessageName = _name}
            -> mconcat
                [ "Of the "
                , _name
                , " dynasty"
                ]
        MsgNewRulerAge {scriptMessageAmt = _amt}
            -> mconcat
                [ "Aged "
                , toMessage (roundNum _amt)
                , " years"
                ]
        MsgNewRulerAdm {scriptMessageFixed = _fixed, scriptMessageAmt = _amt}
            -> mconcat
                [ toMessage (ifThenElseT _fixed "Fixed " "")
                , "{{icon|adm}} "
                , toMessage (roundNum _amt)
                , " administrative skill"
                ]
        MsgNewRulerDip {scriptMessageFixed = _fixed, scriptMessageAmt = _amt}
            -> mconcat
                [ toMessage (ifThenElseT _fixed "Fixed " "")
                , "{{icon|dip}} "
                , toMessage (roundNum _amt)
                , " diplomatic skill"
                ]
        MsgNewRulerMil {scriptMessageFixed = _fixed, scriptMessageAmt = _amt}
            -> mconcat
                [ toMessage (ifThenElseT _fixed "Fixed " "")
                , "{{icon|mil}} "
                , toMessage (roundNum _amt)
                , " military skill"
                ]
        MsgNewRulerClaim {scriptMessageAmt = _amt}
            -> mconcat
                [ "Claim strength "
                , toMessage (roundNum _amt)
                ]
        MsgNewRulerFixed {scriptMessageAdm = _adm, scriptMessageDip = _dip, scriptMessageMil = _mil}
            -> mconcat
                [ "A new ruler comes to power with fixed skills {{icon|adm}} "
                , toMessage (roundNum _adm)
                , ", {{icon|dip}} "
                , toMessage (roundNum _dip)
                , ", {{icon|mil}} "
                , toMessage (roundNum _mil)
                ]
        MsgEstateHasInfluenceModifier {scriptMessageIcon = _icon, scriptMessageEstate = _estate, scriptMessageModifier = _modifier}
            -> mconcat
                [ _icon
                , " "
                , _estate
                , " estate has influence modifier "
                , toMessage (iquotes _modifier)
                ]
        MsgTriggerSwitch
            -> "The first of the following that is true:"
        MsgTriggerSwitchClause {scriptMessageCond = _cond}
            -> mconcat
                [ "If "
                , _cond
                , ":"
                ]
        MsgProvinceHasRebels {scriptMessageIcon = _icon, scriptMessageRtype = _rtype}
            -> mconcat
                [ "Province's most likely rebel type is "
                , _icon
                , " "
                , _rtype
                ]
        MsgRebelsFriendlyTo {scriptMessageFriend = _friend}
            -> mconcat
                [ "friendly to "
                , _friend
                ]
        MsgRebelsLedBy {scriptMessageLeader = _leader}
            -> mconcat
                [ "led by "
                , _leader
                ]
        MsgRebelsGainProgress {scriptMessageAmt = _amt}
            -> mconcat
                [ "gaining "
                , toMessage (roundNum _amt)
                , " progress towards their next uprising"
                ]
        MsgSpawnRebels {scriptMessageRtype = _rtype, scriptMessageSize = _size, scriptMessageFriend = _friend, scriptMessageLeader = _leader, scriptMessageWin = _win, scriptMessageProgress = _progress}
            -> mconcat
                [ _rtype
                , " (size "
                , toMessage (roundNum _size)
                , ")"
                , _friend
                , " rise in revolt"
                , _leader
                , toMessage (ifThenElseT _win " and occupy the province" "")
                , _progress
                ]
        MsgRebelsHaveRisen {scriptMessageIcon = _icon, scriptMessageRtype = _rtype}
            -> mconcat
                [ _icon
                , " "
                , _rtype
                , " have risen in revolt"
                ]
        MsgTagGainsCore {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " gains a core on this province"
                ]
        MsgGainCoreOnProvince {scriptMessageProv = _prov}
            -> mconcat
                [ "Gain core on "
                , _prov
                ]
        MsgHasDLC {scriptMessageIcon = _icon, scriptMessageDlc = _dlc}
            -> mconcat
                [ "DLC "
                , _icon
                , " "
                , _dlc
                , " is active"
                ]
        MsgProvince {scriptMessageWhere = _where}
            -> mconcat
                [ "Province "
                , _where
                , ":"
                ]
        MsgTechGroup {scriptMessageIcon = _icon, scriptMessageName = _name}
            -> mconcat
                [ "Technology group is "
                , _icon
                , " "
                , _name
                ]
        MsgNumOfReligion {scriptMessageIcon = _icon, scriptMessageName = _name, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , toMessage (roundNum _amt)
                , " "
                , plural (round _amt) "province" "provinces"
                , " of "
                , _icon
                , " "
                , _name
                , " religion"
                ]
        MsgStrongestTradePower {scriptMessageWho = _who}
            -> mconcat
                [ _who
                , " is the strongest trade power in this node"
                ]
        MsgAreaIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Is in "
                , _what
                , " area"
                ]
        MsgDominantReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "The country's dominant religion is "
                , _icon
                , " "
                , _what
                ]
        MsgHREReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "The Holy Roman Empire's official religion is "
                , _icon
                , " "
                , _what
                ]
        MsgSetHREReligionLocked {scriptMessageYn = _yn}
            -> mconcat
                [ "The Holy Roman Empire's religion "
                , toMessage (ifThenElseT _yn "becomes fixed" "is no longer fixed")
                ]
        MsgSetHREReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "The Holy Roman Empire's official religion becomes "
                , _icon
                , " "
                , _what
                ]
        MsgSetHREHereticReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "The "
                , _icon
                , " "
                , _what
                , " religion is considered heretical within the Holy Roman Empire"
                ]
        MsgSignWestphalia
            -> "From now on, the Emperor, Electors and Princes of the {{icon|hre}} Holy Roman Empire may be of any Christian religion without being considered heretics."
        MsgHRELeaguesEnabled {scriptMessageYn = _yn}
            -> mconcat
                [ "Leagues for the religion of the Holy Roman Empire "
                , toMessage (ifThenElseT _yn "have" "have ''not''")
                , " begun to form"
                ]
        MsgHREReligionLocked {scriptMessageYn = _yn}
            -> mconcat
                [ "The religion of the Holy Roman Empire "
                , toMessage (ifThenElseT _yn "is" "is ''not''")
                , " fixed"
                ]
        MsgHREWestphalia {scriptMessageYn = _yn}
            -> mconcat
                [ "The Peace of Westphalia "
                , toMessage (ifThenElseT _yn "has" "has ''not''")
                , " been signed"
                ]
        MsgIsElector {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " an Elector of the Holy Roman Empire"
                ]
        MsgNoHREReforms
            -> "No reforms have been passed in the Holy Roman Empire"
        MsgHREPassedReform {scriptMessageWhat = _what}
            -> mconcat
                [ "The imperial reform ''"
                , _what
                , "'' has been passed"
                ]
        MsgEnableHRELeagues
            -> "Leagues for the religion of the Holy Roman Empire begin to form."
        MsgIsInLeagueWar {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " involved in a religious league war"
                ]
        MsgIsLeagueEnemy {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is in the opposite religious league to "
                , _whom
                ]
        MsgReligionYears {scriptMessageIcon = _icon, scriptMessageName = _name, scriptMessageYears = _years}
            -> mconcat
                [ "The "
                , _icon
                , " "
                , _name
                , " religion has existed for at least "
                , toMessage (plainNum _years)
                , " years"
                ]
        MsgHasIdea {scriptMessageWhat = _what}
            -> mconcat
                [ "Has idea "
                , toMessage (iquotes _what)
                ]
        MsgReligionProvinces {scriptMessageIcon = _icon, scriptMessageName = _name, scriptMessageAmt = _amt}
            -> mconcat
                [ "Country has at least "
                , toMessage (roundNum _amt)
                , " "
                , plural (round _amt) "province" "provinces"
                , " of "
                , _icon
                , " "
                , _name
                , " religion"
                ]
        MsgGoodsProvinces {scriptMessageIcon = _icon, scriptMessageName = _name, scriptMessageAmt = _amt}
            -> mconcat
                [ "Country has at least "
                , toMessage (roundNum _amt)
                , " "
                , plural (round _amt) "province" "provinces"
                , " producing "
                , _icon
                , " "
                , _name
                ]
        MsgHasAristocraticIdea {scriptMessageName = _name, scriptMessageNum = _num}
            -> mconcat
                [ "Has {{icon|aristocratic}} Aristocratic idea "
                , toMessage (show _num)
                , " "
                , toMessage (iquotes _name)
                ]
        MsgHasEconomicIdea {scriptMessageName = _name, scriptMessageNum = _num}
            -> mconcat
                [ "Has {{icon|economic}} Economic idea "
                , toMessage (show _num)
                , " "
                , toMessage (iquotes _name)
                ]
        MsgHasDefensiveIdea {scriptMessageName = _name, scriptMessageNum = _num}
            -> mconcat
                [ "Has {{icon|defensive}} Defensive idea "
                , toMessage (show _num)
                , " "
                , toMessage (iquotes _name)
                ]
        MsgHasInnovativeIdea {scriptMessageName = _name, scriptMessageNum = _num}
            -> mconcat
                [ "Has {{icon|innovative}} Innovative idea "
                , toMessage (show _num)
                , " "
                , toMessage (iquotes _name)
                ]
        MsgHasOffensiveIdea {scriptMessageName = _name, scriptMessageNum = _num}
            -> mconcat
                [ "Has {{icon|offensive}} Offensive idea "
                , toMessage (show _num)
                , " "
                , toMessage (iquotes _name)
                ]
        MsgHasMaritimeIdea {scriptMessageName = _name, scriptMessageNum = _num}
            -> mconcat
                [ "Has {{icon|maritime}} Maritime idea "
                , toMessage (show _num)
                , " "
                , toMessage (iquotes _name)
                ]
        MsgColonists {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Colonists"
                ]
        MsgMayExplore
            -> "{{icon|may explore|28px}} Can recruit explorers and conquistadors. Explorers may explore ocean provinces."
        MsgGainColonialRange {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Colonial range"
                ]
        MsgGlobalSettlers {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Global settler increase"
                ]
        MsgGlobalTariffs {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Global tariffs"
                ]
        MsgNavalForcelimitMod {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Naval forcelimit modifier"
                ]
        MsgGainOverseasCB
            -> "Gain permanent \"Overseas Expansion\" [[Casus Belli]] against countries with Indian, Sub-Saharan, Chinese or Nomad tech group. (Only usable if country's tech group is Western, Eastern or Anatolian.)"
        MsgGainPrimitivesCB
            -> "Gain permanent \"Colonial Conquest\" [[Casus Belli]] against all primitives."
        MsgNavyTradition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Naval tradition is at least "
                , toMessage (plainNum _amt)
                ]
        MsgYearlyNavyTradition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Yearly naval tradition"
                ]
        MsgHeavyShipCombatAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Heavy ship combat ability"
                ]
        MsgLightShipCombatAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Light ship combat ability"
                ]
        MsgGalleyCombatAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Galley combat ability"
                ]
        MsgGlobalShipRepair {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Ship repair"
                ]
        MsgGlobalShipCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Ship cost"
                ]
        MsgRegimentCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Regiment cost"
                ]
        MsgNavalLeaderManeuver {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Naval leader maneuver"
                ]
        MsgBlockadeEfficiency {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Blockade efficiency"
                ]
        MsgGainSeaRepair
            -> "Ships repair while in coastal sea provinces."
        MsgPrimitives {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " primitive"
                ]
        MsgGlobalTaxModifier {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Global tax modifier"
                ]
        MsgBuildCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Build cost"
                ]
        MsgYearlyInflationReduction {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign False (negate _amt))
                , " Yearly inflation reduction"
                ]
        MsgInterestPerAnnum {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign False _amt)
                , " Interest per annum"
                ]
        MsgGlobalAutonomy {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign False _amt)
                , " Monthly autonomy change"
                ]
        MsgLandMaintenanceMod {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Land maintenance modifier"
                ]
        MsgNavalMaintenanceMod {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Naval maintenance modifier"
                ]
        MsgProdEff {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Production efficiency"
                ]
        MsgDevelCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " {{DLC-only|Development cost}}"
                ]
        MsgGainReligiousCB
            -> "{{icon|cb on religious enemies|28px}} Gain permanent \"Holy War\" and \"Purging of Heresy\" [[Casus Belli]] against heathens and heretics respectively."
        MsgMissionaries {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Missionaries"
                ]
        MsgStabilityCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Stability cost modifier"
                ]
        MsgMissionaryStrength {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Missionary strength"
                ]
        MsgToleranceHeathen {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Tolerance of heathens"
                ]
        MsgToleranceHeretic {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Tolerance of heretics"
                ]
        MsgToleranceTrue {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Tolerance of the true faith"
                ]
        MsgYearlyPapalInfluence {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Yearly papal influence"
                ]
        MsgYearlyDevotion {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " {{DLC-only|Yearly devotion}}"
                ]
        MsgMonthlyFervor {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " {{DLC-only|Monthly fervor}}"
                ]
        MsgChurchPowerModifier {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " {{DLC-only|Church power}}"
                ]
        MsgPrestige {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Prestige is at least "
                , toMessage (colourNumSign True _amt)
                ]
        MsgYearlyPrestige {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Yearly prestige"
                ]
        MsgMissionaryStrengthVsHeretics {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Missionary strength vs heretics"
                ]
        MsgCultureConvCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Culture conversion cost"
                ]
        MsgHasOpinion {scriptMessageAmt = _amt, scriptMessageWhom = _whom}
            -> mconcat
                [ "Opinion of "
                , _whom
                , " is at least "
                , toMessage (colourNumSign True _amt)
                ]
        MsgNormalOrHistoricalNations {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Playing" "''Not'' playing")
                , " with normal or historical nations"
                ]
        MsgIsCustomNation {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " playing a custom nation"
                ]
        MsgReligionEnabled {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "The "
                , _icon
                , " "
                , _what
                , " religion is enabled"
                ]
        MsgCapitalIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Capital is "
                , _what
                ]
        MsgFullIdeaGroup {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Has completed "
                , _icon
                , " "
                , _what
                ]
        MsgTradeIncomePercentage {scriptMessageAmt = _amt}
            -> mconcat
                [ "Trade accounts for at least "
                , toMessage (reducedNum plainPc _amt)
                , " of national income"
                ]
        MsgReligiousUnity {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " <!-- idea: "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " --> Religious unity is at least "
                , toMessage (reducedNum (colourPc True) _amt)
                ]
        MsgReligiousUnityBonus {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Religious unity"
                ]
        MsgHasADM {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (plainNum _amt)
                , " administrative power"
                ]
        MsgHasDIP {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (plainNum _amt)
                , " diplomatic power"
                ]
        MsgHasMIL {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (plainNum _amt)
                , " military power"
                ]
        MsgRankDuchy
            -> "Government rank is at least {{icon|duchy}} Duchy"
        MsgRankKingdom
            -> "Government rank is {{icon|kingdom}}Kingdom or {{icon|empire}} Empire<!-- not {{icon|duchy}} Duchy -->"
        MsgRankEmpire
            -> "Government rank is {{icon|empire}} Empire<!-- not {{icon|duchy}} Duchy or {{icon|kingdom}} Kingdom -->"
        MsgSetRankDuchy
            -> "Set government rank to {{icon|duchy}} Duchy"
        MsgSetRankKingdom
            -> "Set government rank to {{icon|kingdom}} Kingdom"
        MsgSetRankEmpire
            -> "Set government rank to {{icon|empire}} Empire"
        MsgOverextension {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Overextension is at least "
                , toMessage (reducedNum plainPc _amt)
                ]
        MsgRandomNewWorld {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " playing with a Random New World"
                ]
        MsgIsColonialNation {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a colonial nation"
                ]
        MsgIsFormerColonialNation {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a former colonial nation"
                ]
        MsgIsNomad {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a steppe horde"
                ]
        MsgReligionReformed {scriptMessageYn = _yn}
            -> mconcat
                [ "The country's religion has"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " been reformed"
                ]
        MsgChangeTag {scriptMessageWho = _who}
            -> mconcat
                [ "Country becomes "
                , _who
                ]
        MsgSetInEmpire {scriptMessageYn = _yn}
            -> mconcat
                [ "Province "
                , toMessage (ifThenElseT _yn "joins" "leaves")
                , " the Holy Roman Empire"
                ]
        MsgHasSecondaryReligion {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " a secondary religion"
                ]
        MsgSecondaryReligion {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Secondary religion is "
                , _icon
                , " "
                , _what
                ]
        MsgIsDefenderOfFaith {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " Defender of the Faith"
                ]
        MsgLegitimacyOrHordeUnity {scriptMessageAmt = _amt}
            -> mconcat
                [ "{{icon|legitimacy}} Legitimacy or {{icon|horde unity}} Horde unity is at least "
                , toMessage (roundNum _amt)
                ]
        MsgCheckVariable {scriptMessage_icon = __icon, scriptMessageWhat = _what, scriptMessageAmt = _amt}
            -> mconcat
                [ "Value of variable "
                , _what
                , " is at least "
                , toMessage (plainNum _amt)
                ]
        MsgChangeTechGroup {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Change technology group to "
                , _icon
                , " "
                , _what
                ]
        MsgChangeUnitType {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Change units to "
                , _icon
                , " "
                , _what
                ]
        MsgNoBaseWeight
            -> "(no base weight?)"
        MsgAIBaseWeight {scriptMessageAmt = _amt}
            -> mconcat
                [ "Base weight: "
                , toMessage (plainNum _amt)
                ]
        MsgAIFactorOneline {scriptMessageFactor = _factor, scriptMessageMultiplier = _multiplier}
            -> mconcat
                [ _factor
                , ": "
                , toMessage (bold (plainNum _multiplier))
                ]
        MsgAIFactorHeader {scriptMessageMultiplier = _multiplier}
            -> mconcat
                [ toMessage (bold (plainNum _multiplier))
                , ":"
                ]
        MsgLucky {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " a lucky nation"
                ]
        MsgHasArtistLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Statesman advisor"
                ]
        MsgHasDiplomatLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Statesman advisor"
                ]
        MsgHasArmyReformerLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Army Reformer advisor"
                ]
        MsgHasNaturalScientistLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Natural Scientist advisor"
                ]
        MsgHasNavyReformerLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Navy Reformer advisor"
                ]
        MsgHasTheologianLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Theologian advisor"
                ]
        MsgHasTraderLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Trader advisor"
                ]
        MsgHasStatesmanLevel {scriptMessageIcon = _icon, scriptMessageLevel = _level}
            -> mconcat
                [ "Has a level "
                , toMessage (roundNum _level)
                , " "
                , _icon
                , " Statesman advisor"
                ]
        MsgNumRoyalMarriages {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " royal "
                , plural _amt "marriage" "marriages"
                ]
        MsgIsBankrupt {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " bankrupt"
                ]
        MsgNumColonialSubjects {scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , toMessage (roundNum _amt)
                , " colonial "
                , plural _amt "subject" "subjects"
                ]
        MsgTradeEfficiency {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Trade efficiency is at least "
                , toMessage (reducedNum plainPc _amt)
                ]
        MsgTradeEfficiencyBonus {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPc True) _amt)
                , " Trade efficiency"
                ]
        MsgHasWarTaxes {scriptMessageYn = _yn}
            -> mconcat
                [ "Has"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " raised war taxes"
                ]
        MsgRevoltPercentage {scriptMessageAmt = _amt}
            -> mconcat
                [ "At least "
                , toMessage (roundPc _amt)
                , " of provinces are in revolt"
                ]
        MsgHasAnyDisaster {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " an ongoing disaster"
                ]
        MsgHasActivePolicy {scriptMessageWhat = _what}
            -> mconcat
                [ "Has enacted the policy "
                , toMessage (iquotes _what)
                ]
        MsgHasDucats {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (plainNum _amt)
                , " ducats"
                ]
        MsgHasParliament {scriptMessageYn = _yn}
            -> mconcat
                [ "The country "
                , toMessage (ifThenElseT _yn "has" "does ''not'' have")
                , " a parliament"
                ]
        MsgHasTruce {scriptMessageYn = _yn}
            -> mconcat
                [ "The country "
                , toMessage (ifThenElseT _yn "has" "does ''not'' have")
                , " a truce with another country"
                ]
        MsgNumRebelControlledProvinces {scriptMessageAmt = _amt}
            -> mconcat
                [ "At least "
                , toMessage (plainNum _amt)
                , " "
                , plural _amt "province is" "provinces are"
                , " controlled by rebels"
                ]
        MsgFortLevel {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Fort level is at least "
                , toMessage (roundNum _amt)
                ]
        MsgHasTradeModifier {scriptMessage_icon = __icon, scriptMessageWho = _who, scriptMessageWhat = _what}
            -> mconcat
                [ _who
                , " has trade modifier "
                , toMessage (iquotes _what)
                ]
        MsgIsMonth {scriptMessageWhat = _what}
            -> mconcat
                [ "Month is "
                , _what
                ]
        MsgIsSea {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " sea"
                ]
        MsgHeavyShip {scriptMessageWhom = _whom}
            -> mconcat
                [ "Create a {{icon|heavy ship}} heavy ship belonging to "
                , _whom
                ]
        MsgLightShip {scriptMessageWhom = _whom}
            -> mconcat
                [ "Create a {{icon|light ship}} light ship belonging to "
                , _whom
                ]
        MsgGalley {scriptMessageWhom = _whom}
            -> mconcat
                [ "Create a {{icon|galley}} galley belonging to "
                , _whom
                ]
        MsgNumColonies {scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "colony" "colonies"
                ]
        MsgChangeCulture {scriptMessageWhat = _what}
            -> mconcat
                [ "Change culture to "
                , _what
                ]
        MsgNavalForcelimit {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Naval forcelimit is at least "
                , toMessage (plainNum _amt)
                ]
        MsgBlockade {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Is at least "
                , _icon
                , " "
                , toMessage (plainPc _amt)
                , " blockaded"
                ]
        MsgCreateAlliance {scriptMessageWhom = _whom}
            -> mconcat
                [ "Create alliance with "
                , _whom
                ]
        MsgAddLocalUnrest {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum False _amt)
                , " unrest"
                ]
        MsgGoldIncomePercentage {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "At least "
                , toMessage (reducedNum plainPc _amt)
                , " of the country's income is from "
                , _icon
                , " Gold"
                ]
        MsgIsTribal {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " tribal"
                ]
        MsgSetCapital {scriptMessageWhat = _what}
            -> mconcat
                [ "Change capital to "
                , _what
                ]
        MsgChangePrimaryCulture {scriptMessageWhat = _what}
            -> mconcat
                [ "Change primary culture to "
                , _what
                ]
        MsgColonialRegion {scriptMessageWhere = _where}
            -> mconcat
                [ "Is in colonial region "
                , _where
                ]
        MsgJuniorUnionWith {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is the junior partner in a personal union with "
                , _whom
                ]
        MsgSeniorUnionWith {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is the senior partner in a personal union with "
                , _whom
                ]
        MsgVassalOf {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is a vassal of "
                , _whom
                ]
        MsgOverlordOf {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is the overlord of "
                , _whom
                ]
        MsgChangeProvinceName {scriptMessageWhat = _what}
            -> mconcat
                [ "Rename province to "
                , _what
                ]
        MsgRenameCapital {scriptMessageWhat = _what}
            -> mconcat
                [ "Rename provincial capital to "
                , _what
                ]
        MsgOwnsOrVassal {scriptMessageWhere = _where}
            -> mconcat
                [ "Owns "
                , _where
                , " or is overlord of its owner"
                ]
        MsgIsInColonialRange {scriptMessageWhom = _whom}
            -> mconcat
                [ "Is within colonial range of "
                , _whom
                ]
        MsgConstructingGreatProject {scriptMessageWhat = _what}
            -> mconcat
                [ "Is constructing the "
                , _what
                ]
        MsgConstructing {scriptMessageWhat = _what}
            -> mconcat
                [ "Is constructing a "
                , _what
                ]
        MsgStartConstructingGreatProject {scriptMessageWhat = _what}
            -> mconcat
                [ "Begin constructing the "
                , _what
                ]
        MsgCancelConstruction
            -> "Cancel construction"
        MsgYearsOfIncome {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Treasury contains at least "
                , _icon
                , " "
                , toMessage (plainNum _amt)
                , " "
                , plural _amt "year's" "years'"
                , " worth of income"
                ]
        MsgLibertyDesire {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Liberty desire is at least "
                , toMessage (colourPc False _amt)
                ]
        MsgGainLibertyDesire {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Gain "
                , _icon
                , " "
                , toMessage (colourPc False _amt)
                , " liberty desire"
                ]
        MsgColonialParent
            -> "This country's colonial parent:"
        MsgAlways {scriptMessageYn = _yn}
            -> toMessage (ifThenElseT _yn "{{icon|yes}} Always" "{{icon|no}} Never")
        MsgCapitalCultureDominant
            -> "The capital's {{icon|culture}} culture is dominant in the country"
        MsgNumUnions {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " personal "
                , plural _amt "union" "unions"
                ]
        MsgNumVassals {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Has at least "
                , _icon
                , " "
                , toMessage (roundNum _amt)
                , " "
                , plural _amt "vassal" "vassals"
                ]
        MsgFreeVassal {scriptMessageWhom = _whom}
            -> mconcat
                [ "Release vassal "
                , _whom
                , " as an independent country"
                ]
        MsgHasMissionary {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " an active missionary"
                ]
        MsgNavyPercentage {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ "Navy is at least "
                , toMessage (plainPc _amt)
                , " of "
                , _icon
                , " force limit"
                ]
        MsgWasForceConverted {scriptMessageYn = _yn}
            -> mconcat
                [ "Was"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " force converted"
                ]
        MsgFemaleEmperorAllowed {scriptMessageYn = _yn}
            -> mconcat
                [ "The Emperor may"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " be female"
                ]
        MsgImperialAuthority {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Imperial Authority is at least "
                , toMessage (roundNum _amt)
                ]
        MsgHasFemaleHeir {scriptMessageYn = _yn}
            -> mconcat
                [ toMessage (ifThenElseT _yn "Has" "Does ''not'' have")
                , " a female heir"
                ]
        MsgPiety {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " Piety is at least "
                , toMessage (reducedNum (colourPcSign True) _amt)
                ]
        MsgIsInTutorial {scriptMessageYn = _yn}
            -> mconcat
                [ "Is"
                , toMessage (ifThenElseT _yn "" " ''not''")
                , " in the tutorial"
                ]
        MsgGainFervor {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " fervor"
                ]
        MsgGainChurchPower {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ gainOrLose _amt
                , " "
                , _icon
                , " "
                , toMessage (colourNum True _amt)
                , " church power"
                ]
        MsgIncomeFromVassals {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Income from vassals"
                ]
        MsgTimeToFabricateClaims {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Time to fabricate claims"
                ]
        MsgDiploAnnexCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Diplomatic annexation cost"
                ]
        MsgAEImpact {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Aggressive expansion impact"
                ]
        MsgDiploRep {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Diplomatic reputation"
                ]
        MsgEnvoyTravelTime {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Envoy travel time"
                ]
        MsgDiploRelations {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Diplomatic relations"
                ]
        MsgVassalForcelimitContribution {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Vassal force limit contribution"
                ]
        MsgUnjustifiedDemands {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Unjustified demands"
                ]
        MsgInfantryCombatAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Infantry combat ability"
                ]
        MsgCavalryCombatAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Cavalry combat ability"
                ]
        MsgArtilleryCombatAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Artillery combat ability"
                ]
        MsgShipDurability {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Ship durability"
                ]
        MsgMoraleOfArmies {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Morale of armies"
                ]
        MsgMoraleOfNavies {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Morale of navies"
                ]
        MsgNavalAttrition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Naval attrition"
                ]
        MsgDiscipline {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Discipline"
                ]
        MsgNationalManpowerMod {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " National manpower modifier"
                ]
        MsgManpowerRecoverySpeed {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Manpower recovery speed"
                ]
        MsgAvailableMercs {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Available mercenaries"
                ]
        MsgGarrisonSize {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Garrison size"
                ]
        MsgLandAttrition {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Land attrition"
                ]
        MsgLandForcelimitMod {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Land force limit modifier"
                ]
        MsgPrestigeDecay {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Prestige decay"
                ]
        MsgMercCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Mercenary cost"
                ]
        MsgTechCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Technology cost"
                ]
        MsgPossibleAdvisors {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Possible advisors"
                ]
        MsgReduceInflationCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Reduce inflation cost"
                ]
        MsgLeadersWithoutUpkeep {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Leader(s)<!-- sic --> without upkeep"
                ]
        MsgAdvisorCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Advisor costs"
                ]
        MsgCoreCreationCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Core creation cost"
                ]
        MsgMercMaintenance {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Mercenary maintenance modifier"
                ]
        MsgADMTechCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Administrative technology cost"
                ]
        MsgDIPTechCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Diplomatic technology cost"
                ]
        MsgGoodsProducedMod {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Goods produced modifier"
                ]
        MsgNavalLeaderFire {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Naval leader fire"
                ]
        MsgNavalLeaderShock {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourNumSign True) _amt)
                , " Naval leader shock"
                ]
        MsgPrestigeFromLand {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Prestige from land battles"
                ]
        MsgPrestigeFromNaval {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Prestige from naval battles"
                ]
        MsgDiplomats {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Diplomats"
                ]
        MsgWarExhaustionCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Cost of reducing war exhaustion"
                ]
        MsgImproveRelations {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Improve relations"
                ]
        MsgProvinceWarscoreCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Province warscore cost"
                ]
        MsgReducedStabImpacts
            -> "Lowered impact on stability from diplomatic actions"
        MsgNationalUnrest {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign False _amt)
                , " National unrest"
                ]
        MsgYearsOfSeparatism {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign False _amt)
                , " Years of separatism"
                ]
        MsgAccCultureThreshold {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Accepted culture threshold"
                ]
        MsgBetterRelationsOverTime {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Better relations over time"
                ]
        MsgIdeaCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Idea cost"
                ]
        MsgMerchants {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Merchants"
                ]
        MsgRecruitmentTime {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Recruitment time"
                ]
        MsgShipbuildingTime {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Shipbuilding time"
                ]
        MsgGlobalTradePower {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Global trade power"
                ]
        MsgEmbargoEff {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Embargo efficiency"
                ]
        MsgPrivateerEff {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Privateer efficiency"
                ]
        MsgGlobalSpyDefence {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " National spy defence"
                ]
        MsgMaySabotageReputation
            -> "{{icon|may sabotage reputation|28px}} May [[sabotage reputation]]"
        MsgSpyOffense {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Spy offense"
                ]
        MsgCovertActionRelationImpact {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Covert action relation impact"
                ]
        MsgMayStudyTech
            -> "{{icon|may study technology|28px}} May [[study technology]]"
        MsgMaySowDiscontent
            -> "{{icon|may sow discontent|28px}} May [[sow discontent]]"
        MsgMayAgitateForLiberty
            -> "May [[agitate for liberty]]"
        MsgMayInfiltrateAdministration
            -> "{{icon|may infiltrate administration|28px}} May [[infiltrate administration]]"
        MsgRebelSupportEff {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Rebel support efficiency"
                ]
        MsgArmyTraditionDecay {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Army tradition decay"
                ]
        MsgNavyTraditionDecay {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Navy tradition decay"
                ]
        MsgInfantryCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Infantry cost"
                ]
        MsgCavalryCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Cavalry cost"
                ]
        MsgMILTechCost {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Military technology cost"
                ]
        MsgHostileCoreCreation {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Hostile core-creation cost on us"
                ]
        MsgCaravanPower {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Caravan power"
                ]
        MsgLandLeaderFire {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Land leader fire"
                ]
        MsgLandLeaderShock {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Land leader shock"
                ]
        MsgLandLeaderManeuver {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Land leader maneuver"
                ]
        MsgLeaderSiege {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Leader siege"
                ]
        MsgFortDefense {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourPcSign True _amt)
                , " Fort defense"
                ]
        MsgFortMaintenance {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign False) _amt)
                , " Fort maintenance"
                ]
        MsgReinforceSpeed {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourPcSign True _amt)
                , " Reinforce speed"
                ]
        MsgAttritionForEnemies {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (colourNumSign True _amt)
                , " Attrition for enemies"
                ]
        MsgSiegeAbility {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Siege ability"
                ]
        MsgRecoverArmyMoraleSpeed {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Recover army morale speed"
                ]
        MsgTradeRange {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Trade range"
                ]
        MsgTradeSteering {scriptMessageIcon = _icon, scriptMessageAmt = _amt}
            -> mconcat
                [ _icon
                , " "
                , toMessage (reducedNum (colourPcSign True) _amt)
                , " Trade steering"
                ]
        MsgTextIs {scriptMessageWhat = _what}
            -> mconcat
                [ "Text is: ''"
                , _what
                , "''"
                ]
        MsgAnyOwnedPlanet
            -> "Any owned planet:"
        MsgAnyOwnedShip
            -> "Any owned ship:"
        MsgAnyPop
            -> "Any pop:"
        MsgEveryOwnedPlanet
            -> "Every owned planet:"
        MsgEveryOwnedPop
            -> "Every pop on owned planets:"
        MsgRandomOwnedShip
            -> "One random owned ship:"
        MsgRandomPop
            -> "One random pop:"
        MsgRandomSystem
            -> "One random system:"
        MsgRandomTile
            -> "One random tile:"
        MsgGainTrait {scriptMessageIcon = _icon, scriptMessageWhat = _what}
            -> mconcat
                [ "Gain trait "
                , _icon
                , " "
                , _what
                ]

type IndentedMessage = (Int, ScriptMessage)
type IndentedMessages = [IndentedMessage]

messageText :: Monad m => ScriptMessage -> PPT m Text
messageText msg = do
    mlangs <- getLangs
    return $ renderMessage Script mlangs msg

message :: Monad m => ScriptMessage -> PPT m Doc
message msg = strictText <$> messageText msg

imsg2doc :: Monad m => IndentedMessages -> PPT m Doc
imsg2doc msgs = vsep <$>
                mapM (\(i,rm) -> do
                        m <- message rm
                        return (hsep [strictText (T.replicate i "*"),  m]))
                     msgs

-- Use HTML to format the messages instead of wiki markup. This behaves better
-- with <pre> blocks but doesn't play well with idea groups.
imsg2doc_html :: forall m. Monad m => IndentedMessages -> PPT m Doc
imsg2doc_html [] = return mempty
imsg2doc_html msgs@((i,_):_)
    | i > 0     = enclose "<ul>" "</ul>" . fst <$> imsg2doc' msgs
    | otherwise = fst <$> imsg2doc' msgs
    where
        -- Format all (remaining) messages at the current indent level.
        imsg2doc' :: IndentedMessages -> PPT m (Doc, IndentedMessages)
        imsg2doc' [] = return (mempty, [])
        imsg2doc' [(_, rm)] = do -- Last message.
            m <- message rm
            return (enclose "<li>" "</li>" m, [])
        imsg2doc' ((i, rm):msgs@((i',_):_))
            | i < i' = do
                -- New indent.
                m <- message rm
                -- Format the indented stuff.
                (indented, moremsgs) <- imsg2doc' msgs
                -- Format stuff after the indent.
                (postdoc, restmsgs) <- imsg2doc' moremsgs
                -- Put it all together.
                return (vsep
                            [enclose "<li>" "</li>"
                                (vsep
                                    [m
                                    ,enclose "<ul>" "</ul>" indented])
                            ,postdoc]
                       , restmsgs)
            | i > i' = do
                -- Last message at this level.
                m <- enclose "<li>" "</li>" <$> message rm
                return (m, msgs)
            | otherwise = do
                -- Carry on with this indent level.
                m <- enclose "<li>" "</li>" <$> message rm
                (postdoc, restmsgs) <- imsg2doc' msgs
                return (m <> line <> postdoc, restmsgs)

