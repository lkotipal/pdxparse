{-|
Module      : HOI4.Types
Description : Types specific to Hearts of Iron IV
-}
module HOI4.Types (
        -- * Parser state
        HOI4Data (..), HOI4State (..)
    ,   HOI4Info (..)
        -- * Features
    ,   HOI4EvtTitle (..), HOI4EvtDesc (..), HOI4Event (..), HOI4Option (..)
    ,   HOI4EventSource (..), HOI4EventTriggers, HOI4EventWeight
    ,   HOI4Decision (..), HOI4DecisionCost(..), HOI4DecisionIcon(..), HOI4Decisioncat (..)
    ,   IdeaGroup (..), Idea (..), IdeaTable
--    ,   HOI4Modifier (..)
    ,   HOI4OpinionModifier (..)
--    ,   HOI4MissionTreeBranch (..), HOI4Mission (..)
--    ,   HOI4ScopeStateTriggeredModifier (..)
    ,   HOI4NationalFocus (..)
        -- * Low level types
    ,   MonarchPower (..)
    ,   HOI4Scope (..)
    ,   AIWillDo (..)
    ,   AIModifier (..)
    ,   HOI4GeoType (..)
    ,   aiWillDo
    ,   isGeographic
    -- utilities that can't go anywhere else
--    ,   getModifier
    ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Abstract -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings
                     , IsGame (..), IsGameData (..), IsGameState (..))
--import Doc

--------------------------------------------
-- Types used by toplevel Settings module --
--------------------------------------------

-- | Settings, raw scripts, and parsed scripts.
data HOI4Data = HOI4Data {
        hoi4settings :: Settings
    ,   hoi4events :: HashMap Text HOI4Event
    ,   hoi4decisioncats :: HashMap Text HOI4Decisioncat
    ,   hoi4decisions :: HashMap Text HOI4Decision
    ,   hoi4ideaGroups :: IdeaTable
--    ,   hoi4modifiers :: HashMap Text HOI4Modifier
    ,   hoi4opmods :: HashMap Text HOI4OpinionModifier
--    ,   hoi4missions :: HashMap Text HOI4MissionTreeBranch
    ,   hoi4eventTriggers :: HOI4EventTriggers
    ,   hoi4geoData :: HashMap Text HOI4GeoType
--    ,   hoi4provtrigmodifiers :: HashMap Text HOI4ScopeStateTriggeredModifier
    ,   hoi4eventScripts :: HashMap FilePath GenericScript
    ,   hoi4decisioncatScripts :: HashMap FilePath GenericScript
    ,   hoi4decisionScripts :: HashMap FilePath GenericScript
    ,   hoi4ideaGroupScripts :: HashMap FilePath GenericScript
--    ,   hoi4modifierScripts :: HashMap FilePath GenericScript
    ,   hoi4opmodScripts :: HashMap FilePath GenericScript
--    ,   hoi4missionScripts :: HashMap FilePath GenericScript
    ,   hoi4onactionsScripts :: HashMap FilePath GenericScript
--    ,   hoi4disasterScripts :: HashMap FilePath GenericScript
--    ,   hoi4provtrigmodifierScripts :: HashMap FilePath GenericScript
    ,   hoi4tradeNodes :: HashMap Int Text -- Province Id -> Non localized provice name
    ,   hoi4extraScripts :: HashMap FilePath GenericScript -- Extra scripts parsed on the command line
    ,   hoi4extraScriptsCountryScope :: HashMap FilePath GenericScript -- Extra scripts parsed on the command line
    ,   hoi4extraScriptsProvinceScope :: HashMap FilePath GenericScript -- Extra scripts parsed on the command line
    ,   hoi4extraScriptsModifier :: HashMap FilePath GenericScript -- Extra scripts parsed on the command line
    ,   hoi4nationalfocusScripts :: HashMap FilePath GenericScript
    -- etc.
    }

-- | State type for HOI4.
data HOI4State = HOI4State {
        hoi4scopeStack :: [HOI4Scope]
    ,   hoi4currentFile :: Maybe FilePath
    ,   hoi4currentIndent :: Maybe Int
    ,   hoi4IsInEffect :: Bool
    } deriving (Show)

-- | Interface for HOI4 feature handlers. Most of the methods just get data
-- tables from the parser state. These are empty until the relevant parsing
-- stages have been done. In order to avoid import loops, handlers don't know
-- the 'HOI4.Settings.HOI4' type itself, only its instances.
class (IsGame g,
       Scope g ~ HOI4Scope,
       IsGameData (GameData g),
       IsGameState (GameState g)) => HOI4Info g where
    -- | Get the title of an event by its ID. Only works if event scripts have
    -- been parsed.
    getEventTitle :: Monad m => Text -> PPT g m (Maybe Text)
    -- | Get the contents of all event script files.
    getEventScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Save (or amend) the contents of script event files in state.
    setEventScripts :: Monad m => HashMap FilePath GenericScript -> PPT g m ()
    -- | Get the parsed events table (keyed on event ID).
    getEvents :: Monad m => PPT g m (HashMap Text HOI4Event)
    -- | Get the contents of all idea groups files.
    getIdeaGroupScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed idea groups table (keyed on idea group ID).
    getIdeaGroups :: Monad m => PPT g m IdeaTable
    -- | Get the contents of all modifier script files.
--    getModifierScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed modifiers table (keyed on modifier ID).
--    getModifiers :: Monad m => PPT g m (HashMap Text HOI4Modifier)
    -- | Get the contents of all opinion modifier script files.
    getOpinionModifierScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed opinion modifiers table (keyed on modifier ID).
    getOpinionModifiers :: Monad m => PPT g m (HashMap Text HOI4OpinionModifier)
    -- | Get the contents of all decision categories script files.
    getDecisioncatScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the contents of all decision script files.
    getDecisionScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed decision categories table (keyed on decision category ID).
    getDecisioncats :: Monad m => PPT g m (HashMap Text HOI4Decisioncat)
    -- | Get the parsed decisions table (keyed on decision ID).
    getDecisions :: Monad m => PPT g m (HashMap Text HOI4Decision)
    -- | Get the contents of all mission script files
--    getMissionScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed mission trees
--    getMissions :: Monad m => PPT g m (HashMap Text HOI4MissionTreeBranch)
    -- | Get the (known) event triggers
    getEventTriggers :: Monad m => PPT g m HOI4EventTriggers
    -- | Get the on actions script files
    getOnActionsScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the on disaster script files
--    getDisasterScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed geographic data
    getGeoData :: Monad m => PPT g m (HashMap Text HOI4GeoType)
    -- | Get the contents of all province triggered modifier script files.
--    getProvinceTriggeredModifierScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed province triggered modifiers table (keyed on modifier ID).
--    getProvinceTriggeredModifiers :: Monad m => PPT g m (HashMap Text HOI4ScopeStateTriggeredModifier)
    -- | Get the trade nodes
    getTradeNodes :: Monad m => PPT g m (HashMap Int Text)
    -- | Get extra scripts parsed from command line arguments
    getExtraScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    getExtraScriptsCountryScope :: Monad m => PPT g m (HashMap FilePath GenericScript)
    getExtraScriptsProvinceScope :: Monad m => PPT g m (HashMap FilePath GenericScript)
    getExtraScriptsModifier :: Monad m => PPT g m (HashMap FilePath GenericScript)
    getNationalFocusScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)

-------------------
-- Feature types --
-------------------

-- | Event title type. As of HoI4 whatever version, titles may be conditional.
data HOI4EvtTitle
    = HOI4EvtTitleSimple Text  -- title = key
    | HOI4EvtTitleConditional GenericScript Text
            -- title = { text = key trigger = conditions }
    | HOI4EvtTitleCompound GenericScript
            -- title = { trigger = { conditional_expressions } }
    deriving (Show)

-- | Event description type. As of HOI4 whatever version, descriptions may be conditional.
data HOI4EvtDesc
    = HOI4EvtDescSimple Text  -- desc = key
    | HOI4EvtDescConditional GenericScript Text
            -- desc = { text = key trigger = conditions }
    | HOI4EvtDescCompound GenericScript
            -- desc = { trigger = { conditional_expressions } }
    deriving (Show)

-- | Event data.
data HOI4Event = HOI4Event {
    -- | Event ID
        hoi4evt_id :: Maybe Text
    -- | Event title l10n key
    ,   hoi4evt_title :: [HOI4EvtTitle]
    -- | Description
    ,   hoi4evt_desc :: [HOI4EvtDesc]
--  -- | Event picture
--  ,   hoi4evt_picture :: Maybe Text
    -- | Type of thing the event happens to (e.g.  for a @country_event@ this
    -- is 'HOI4Country'). This is used to set the top level scope for its
    -- scripts.
    ,   hoi4evt_scope :: HOI4Scope
    -- | What conditions allow the event to trigger.
    ,   hoi4evt_trigger :: Maybe GenericScript
    -- | Whether the event is only triggered by script commands. If this is
    -- @False@ and the event also has a @mean_time_to_happen@, it can happen
    -- randomly.
    ,   hoi4evt_is_triggered_only :: Maybe Bool
    -- | If this is a random event, how unlikely this event is to happen.
    ,   hoi4evt_mean_time_to_happen :: Maybe GenericScript
    -- | Commands to execute as soon as the event fires.
    ,   hoi4evt_immediate :: Maybe GenericScript
    -- | Whether this is a hidden event (it will have no options).
    ,   hoi4evt_hide_window :: Bool
    -- | Whether this event can only happen once per campaign
    ,   hoi4evt_fire_only_once :: Bool
    -- | List of options for the player/AI to choose from.
    ,   hoi4evt_options :: Maybe [HOI4Option]
    -- | If the event show to sender
    ,   hoi4evt_fire_for_sender :: Maybe Bool
    -- | Effects that take place after any option is selected.
    ,   hoi4evt_after :: Maybe GenericScript
    -- | The event's source file.
    ,   hoi4evt_path :: FilePath
    } deriving (Show)
-- | Event option data.
data HOI4Option = HOI4Option
    {   hoi4opt_name :: Maybe Text               -- ^ Text of the option
    ,   hoi4opt_trigger :: Maybe GenericScript   -- ^ Condition for the option to be available
    ,   hoi4opt_ai_chance :: Maybe AIWillDo -- ^ Probability that the AI will choose this option
    ,   hoi4opt_effects :: Maybe GenericScript   -- ^ What happens if the player/AI chooses this option
    } deriving (Show)

type HOI4EventWeight = Maybe (Integer, Integer) -- Rational reduces the number, which we don't want

data HOI4EventSource =
      HOI4EvtSrcImmediate Text                       -- Immediate effect of an event (arg is event ID)
    | HOI4EvtSrcAfter Text                           -- After effect of an event (arg is event ID)
    | HOI4EvtSrcOption Text Text                     -- Effect of choosing an event option (args are event ID and option ID)
    | HOI4EvtSrcDecComplete Text Text                   -- Effect of completing a decision (args are id and localized decision text)
    | HOI4EvtSrcDecRemove Text Text                   -- Effect of taking a timed decision and letting it finish (args are id and localized decision text)
    | HOI4EvtSrcDecCancel Text Text                   -- Effect of taking a decision and it being canceled (args are id and localized decision text)
    | HOI4EvtSrcDecTimeout Text Text                   -- Effect of taking a decision/mission and letting it timeout (args are id and localized decision text)
    | HOI4EvtSrcOnAction Text HOI4EventWeight         -- An effect from on_actions (args are the trigger and weight)
--    | HOI4EvtSrcDisaster Text Text HOI4EventWeight    -- Effect of a disaster (args are id, trigger and weight)
--    | HOI4EvtSrcMission Text                         -- Effect of completing a mission (arg is the mission id)
    | HOI4EvtSrcNFComplete Text Text                -- Effect of completing a national focus
    | HOI4EvtSrcNFSelect Text Text                  -- Effect of selecting a national focus
    deriving Show

type HOI4EventTriggers = HashMap Text [HOI4EventSource]

-- | Table of idea groups, keyed by ID (e.g. @administrative_ideas@).
type IdeaTable = HashMap Text IdeaGroup
-- | Idea group data.
data IdeaGroup = IdeaGroup
    {   ig_name :: Text -- ^ Name of the idea group
    ,   ig_name_loc :: Text -- ^ Localized name of the idea group (in the best language)
    ,   ig_category :: Maybe MonarchPower -- ^ Which type of monarch power is used to buy these ideas
    ,   ig_start :: Maybe GenericScript -- ^ Traditions for a country idea group
    ,   ig_bonus :: Maybe GenericScript -- ^ Finisher / ambitions
    ,   ig_trigger :: Maybe GenericScript -- ^ Availability conditions if any
    ,   ig_free :: Bool -- ^ Whether this is a country idea group
    ,   ig_ideas :: [Idea] -- ^ List of ideas (there should always be 7)
    ,   ig_ai_will_do :: Maybe AIWillDo -- ^ Factors affecting whether AI will choose this group
    ,   ig_path :: Maybe FilePath -- ^ Source file
    } deriving (Show)
-- | Idea data.
data Idea = Idea
    {   idea_name :: Text -- ^ Idea ID
    ,   idea_name_loc :: Text -- ^ Localized idea name
    ,   idea_effects :: GenericScript -- ^ Idea effects (bonus scope)
    } deriving (Show)

-- | Decision data.
data HOI4Decisioncat = HOI4Decisioncat
    {   decc_name :: Text -- ^ Decision category ID
    ,   decc_name_loc :: Maybe Text -- ^ Localized decision category name
    ,   decc_desc_loc :: Maybe Text
    ,   decc_icon :: Maybe Text
    ,   decc_picture :: Maybe Text
    ,   decc_custom_icon :: Maybe GenericScript
    ,   decc_visible :: Maybe GenericScript
    ,   decc_available :: Maybe GenericScript
    ,   decc_visiblity_type :: Maybe Text
    ,   decc_priority :: Maybe Text -- ^ Determines place on the list
    ,   decc_allowed :: Maybe GenericScript -- ^ Conditions that allow the category to appear
    ,   decc_visible_when_empty :: Maybe Text
    ,   decc_scripted_gui :: Maybe Text
    ,   decc_highlight_states :: Maybe GenericScript
    ,   decc_on_map_area :: Maybe GenericScript
    ,   decc_path :: FilePath -- ^ Source file
    } deriving (Show)

data HOI4DecisionCost
    = HOI4DecisionCostSimple Double
    | HOI4DecisionCostVariable Text
    deriving Show

data HOI4DecisionIcon
    = HOI4DecisionIconSimple Text
    | HOI4DecisionIconScript GenericScript
    deriving Show

-- | Decision data.
data HOI4Decision = HOI4Decision
    {   dec_name :: Text -- ^ Decision ID
    ,   dec_name_loc :: Text -- ^ Localized decision name
    ,   dec_desc :: Maybe Text -- ^ Descriptive text (shown on hover)
    ,   dec_icon :: Maybe HOI4DecisionIcon -- ^ Icon for the decision
    ,   dec_allowed :: Maybe GenericScript -- ^ Conditions that allow the player/AI to
                                           --   take the decision
    ,   dec_target_root_trigger :: Maybe GenericScript
    ,   dec_visible :: Maybe GenericScript
    ,   dec_available :: Maybe GenericScript
    ,   dec_complete_effect :: Maybe GenericScript -- ^ the block of effects that gets executed immediately
                                                   --   when the decision is selected (Starting the timer if it has one).
    ,   dec_days_re_enable :: Maybe Double
    ,   dec_fire_only_once :: Bool
    ,   dec_cost :: Maybe HOI4DecisionCost
    ,   dec_custom_cost_trigger  :: Maybe GenericScript
    ,   dec_custom_cost_text :: Maybe Text
    ,   dec_days_remove :: Maybe Double
    ,   dec_remove_effect :: Maybe GenericScript
    ,   dec_modifier :: Maybe GenericScript
    ,   dec_cancel_trigger ::  Maybe GenericScript
    ,   dec_cancel_effect ::  Maybe GenericScript
    ,   dec_war_with_on_remove :: Maybe Text
    ,   dec_war_with_on_complete :: Maybe Text

    ,   dec_days_mission_timeout :: Maybe Double
    ,   dec_activation :: Maybe GenericScript
    ,   dec_timeout_effect :: Maybe GenericScript

    ,   dec_targets :: Maybe GenericScript
    ,   dec_target_array :: Maybe GenericScript
    ,   dec_targets_dynamic :: Bool
    ,   dec_target_trigger :: Maybe GenericScript
    ,   dec_war_with_target_on_complete :: Bool
    ,   dec_war_with_target_on_remove :: Bool
    ,   dec_war_with_target_on_timeout :: Bool

    ,   dec_state_target :: Bool
    ,   dec_on_map_mode :: Maybe Text
    ,   dec_ai_will_do :: Maybe AIWillDo -- ^ Factors affecting whether an AI
                                         --   will take the decision when available
    ,   dec_path :: Maybe FilePath -- ^ Source file
    ,   dec_cat :: Text -- ^ Category of the decision
    } deriving (Show)
{-
data HOI4Modifier = HOI4Modifier
    {   modName :: Text
    ,   modLocName :: Maybe Text
    ,   modPath :: FilePath
    ,   modReligious :: Bool
    ,   modEffects :: GenericScript
    } deriving (Show)

data HOI4ScopeStateTriggeredModifier = HOI4ScopeStateTriggeredModifier
    {   ptmodName :: Text
    ,   ptmodLocName :: Maybe Text
    ,   ptmodPath :: FilePath
    ,   ptmodEffects :: GenericScript        -- The modifier to apply when the triggered modifier is active
    ,   ptmodPotential :: GenericScript      -- Whether the triggered modifier is visible in the Province view window
    ,   ptmodTrigger :: GenericScript        -- Whether the triggered modifier is active
    ,   ptmodOnActivation :: GenericScript   -- Effects to execute when the triggered modifiers switches to active (province scope)
    ,   ptmodOnDeactivation :: GenericScript -- Effects to execute when the triggered modifiers switches to inactive
    } deriving (Show)
-}
data HOI4OpinionModifier = HOI4OpinionModifier
    {   omodName :: Text
    ,   omodLocName :: Maybe Text
    ,   omodPath :: FilePath
    ,   omodValue :: Maybe Double
    ,   omodMax :: Maybe Double
    ,   omodMin :: Maybe Double
    ,   omodDecay :: Maybe Double
    ,   omodDays :: Maybe Double
    ,   omodMonths :: Maybe Double
    ,   omodYears :: Maybe Double
    ,   omodTrade :: Maybe Bool
    ,   omodTarget :: Maybe Bool
    } deriving (Show)

{-
data HOI4Mission = HOI4Mission
    {   hoi4m_id :: Text
    ,   hoi4m_icon :: Text
    ,   hoi4m_slot :: Int -- Which column (1..5) does the mission tree branch appear in?
    ,   hoi4m_position :: Int -- Which row the mission appears in. 1 is top.
    ,   hoi4m_prerequisites :: [Text]
    ,   hoi4m_trigger :: GenericScript
    ,   hoi4m_effect :: GenericScript
    } deriving (Show)

data HOI4MissionTreeBranch = HOI4MissionTreeBranch
    {   hoi4mtb_path :: FilePath
    ,   hoi4mtb_id :: Text
    ,   hoi4mtb_slot :: Int -- Which column (1..5) does the mission tree branch appear in?
    ,   hoi4mtb_potential :: Maybe GenericScript
    ,   hoi4mtb_missions :: [HOI4Mission]
    } deriving (Show)
-}
data HOI4NationalFocus = HOI4NationalFocus
    {   nf_id :: Text
    ,   nf_name_loc :: Text
    ,   nf_name_desc :: Maybe Text
    ,   nf_text :: Maybe Text
    ,   nf_icon :: Maybe Text
    ,   nf_cost :: Double
    ,   nf_allow_branch  :: Maybe Text
    ,   nf_prerequisite  :: Maybe Text
    ,   nf_mutually_exclusive :: Maybe Text
    ,   nf_avaiable :: Maybe Text
    ,   nf_bypass :: Maybe Text
    ,   nf_cancel :: Maybe Text
    ,   nf_cancelable :: Maybe Text
    ,   nf_historical_ai :: Maybe Text
    ,   nf_available_if_capitulated :: Maybe Text
    ,   nf_cancel_if_invalid :: Maybe Text
    ,   nf_continue_if_invalid :: Maybe Text
    ,   nf_will_lead_to_war_with :: Maybe Text
    ,   nf_search_filters :: Maybe Text
    ,   nf_select_effect :: Maybe GenericScript
    ,   nf_ai_will_do :: Maybe Text
    ,   nf_completion_reward :: Maybe GenericScript
    ,   nf_complete_tooltip :: Maybe Text
    ,   nf_path :: Maybe FilePath -- ^ Source file
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

-- | Types of monarch power.
data MonarchPower = Administrative
                  | Diplomatic
                  | Military
    deriving (Show, Eq, Ord, Generic)
instance Hashable MonarchPower

-- | Scopes
data HOI4Scope
    = HOI4NoScope
    | HOI4Country
    | HOI4ScopeState
    | HOI4TradeNode
    | HOI4Geographic -- ^ Area, etc.
    | HOI4Bonus
    | HOI4UnitLeader
    | HOI4Operative
    | HOI4DualScope
    | HOI4From -- ^ Usually country or province, varies by context
    deriving (Show, Eq, Ord, Enum, Bounded)

data HOI4GeoType
    = HOI4GeoArea
    | HOI4GeoRegion
    | HOI4GeoSuperRegion
    | HOI4GeoContinent
    | HOI4GeoTradeCompany
    | HOI4GeoColonialRegion
    -- Province groups aren't used in the base game (as of 1.30.6)
    deriving (Show)

-- | AI decision factors.
data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    } deriving (Show)
-- | Modifiers for AI decision factors.
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_add :: Maybe Double
    ,   aim_triggers :: GenericScript
    } deriving (Show)
-- | Empty decision factor.
newAIWillDo :: AIWillDo
newAIWillDo = AIWillDo Nothing []
-- | Empty modifier.
newAIModifier :: AIModifier
newAIModifier = AIModifier Nothing Nothing []

-- | Parse an @ai_will_do@ clause.
aiWillDo :: GenericScript -> AIWillDo
aiWillDo = foldl' aiWillDoAddSection newAIWillDo
aiWillDoAddSection :: AIWillDo -> GenericStatement -> AIWillDo
aiWillDoAddSection awd [pdx| $left = %right |] = case T.toLower left of
    "base" -> case floatRhs right of
        Just fac -> awd { awd_base = Just fac }
        _        -> awd
    "factor" -> case floatRhs right of
        Just fac -> awd { awd_base = Just fac }
        _        -> awd
    "modifier" -> case right of
        CompoundRhs scr -> awd { awd_modifiers = awd_modifiers awd ++ [awdModifier scr] }
        _               -> awd
    _ -> awd
aiWillDoAddSection awd _ = awd

-- | Parse a @modifier@ subclause for an @ai_will_do@ clause.
awdModifier :: GenericScript -> AIModifier
awdModifier = foldl' awdModifierAddSection newAIModifier
awdModifierAddSection :: AIModifier -> GenericStatement -> AIModifier
awdModifierAddSection aim stmt@[pdx| $left = %right |] = case T.toLower left of
    "factor" -> case floatRhs right of
        Just fac -> aim { aim_factor = Just fac }
        Nothing  -> aim
    "add" -> case floatRhs right of
        Just add -> aim { aim_add = Just add }
        Nothing  -> aim
    _ -> -- the rest of the statements are just the conditions.
        aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim stmt@[pdx| $left > %right |] = aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim stmt@[pdx| $left < %right |] = aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim _ = aim

isGeographic :: HOI4Scope -> Bool
isGeographic HOI4ScopeState = True
isGeographic HOI4TradeNode = True
isGeographic HOI4Geographic = True
isGeographic _ = False

-----------------------------
-- Miscellaneous utilities --
-----------------------------

--getModifier :: (HOI4Info g, Monad m) => Text -> PPT g m (Maybe HOI4Modifier)
--getModifier id = HM.lookup id <$> getModifiers
