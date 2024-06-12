{-|
Module      : EU4.Scripted
Description : Scripted triggers and effects
-}
module EU4.Scripted (
        parseEU4ScriptedEffects , writeEU4ScriptedEffects
    ) where

import Control.Arrow ((&&&))
import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Maybe (catMaybes, fromMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Data.Void (Void)

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.Regex.TDFA
import qualified Text.Regex.TDFA as RE

import Abstract -- everything
import qualified Doc
import EU4.Common -- everything
import FileIO (Feature (..), writeFeatures)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), IsGameData (..), IsGameState (..), GameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import EU4.Types -- everything

import Debug.Trace (trace, traceM)
import Doc (strictText)
import MessageTools (ToMessage(toMessage))

parseEU4ScriptedEffects :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript
    -> PPT g m (HashMap Text EU4Scripted)
parseEU4ScriptedEffects scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseEU4Scripted scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing scripted effects: " ++ T.unpack err
            return HM.empty
        Right modifiersFilesOrErrors ->
            flip HM.traverseWithKey modifiersFilesOrErrors $ \sourceFile emods ->
                fmap (mkModMap . catMaybes) . forM emods $ \case
                    Left err -> do
                        traceM $ "Error parsing scripted effects in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right mmod -> return mmod
                where mkModMap :: [EU4Scripted] -> HashMap Text EU4Scripted
                      mkModMap = HM.fromList . map (scrName &&& id)

parseEU4Scripted :: (IsGameData (GameData g), IsGameState (GameState g), MonadError Text m)
    -- => (Statement Text Void)
    => GenericStatement
    -> PPT g m (Either Text (Maybe EU4Scripted))
parseEU4Scripted [pdx| $effectid = @effects |]
    = withCurrentFile $ \file -> do
        return . Right . Just $ EU4Scripted {
                scrName = effectid
            ,   scrPath = file
            ,   scrScript = effects
            ,   scrScope = Nothing
            ,   scrRootScope = Nothing
            }
parseEU4Scripted _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for scripted effect in " <> T.pack file)

writeEU4ScriptedEffects :: (EU4Info g, MonadIO m) => PPT g m ()
writeEU4ScriptedEffects = do
    scriptedEffects <- getScriptedEffects
    let scriptedEffectsWithoutParameters = HM.filter (not . effectHasParameters) scriptedEffects
        features :: [Feature EU4Scripted]
        features = map (\effect -> Feature { featurePath = Just (scrPath effect)
                                    , featureId = Just (scrName effect)
                                    , theFeature = Right effect })
                    (HM.elems scriptedEffectsWithoutParameters)
    writeFeatures "scripted_effects" features pp_effect
    where
        effectHasParameters :: EU4Scripted -> Bool
        effectHasParameters effect = toMessage (genericScript2doc (scrScript effect)) =~ ("(\\[\\[|\\$.*\\$)" :: Text) :: Bool
        pp_effect :: (EU4Info g, Monad m) => EU4Scripted -> PPT g m Doc
        pp_effect effect = setCurrentFile (scrPath effect) $ do
            let ascope = fromMaybe EU4Country (scrScope effect)
                rootScope = fromMaybe EU4Country (scrRootScope effect)
            effectText <- setIsInEffect True (scope rootScope (scope ascope (ppScript (scrScript effect))))
            version <- gets (gameVersion . getSettings)
            -- mediawiki does not allow include loops, so we have to use the SEffec2 templae when calling an effect within another scripted effect
            let effectText' = T.replace "{{SEffect|" "{{SEffec2|" (toMessage effectText)
            return $ mconcat [
                        "{{SVersion|", Doc.strictText version, "}}", PP.line,
                        "<section begin=", Doc.strictText (scrName effect), "/>",
                        Doc.strictText effectText',
                        "<section end=", Doc.strictText (scrName effect), "/>"
                    ]
