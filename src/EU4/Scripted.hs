{-|
Module      : EU4.Scripted
Description : Scripted triggers and effects
-}
module EU4.Scripted (
        parseEU4ScriptedEffects
    ) where

import Control.Arrow ((&&&))
import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))

import Data.Maybe (catMaybes)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Data.Void (Void)

import Abstract -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), IsGameData (..), IsGameState (..), GameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import EU4.Types -- everything

import Debug.Trace (trace, traceM)

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
            traceM $ "Completely failed parsing modifiers: " ++ T.unpack err
            return HM.empty
        Right modifiersFilesOrErrors ->
            flip HM.traverseWithKey modifiersFilesOrErrors $ \sourceFile emods ->
                fmap (mkModMap . catMaybes) . forM emods $ \case
                    Left err -> do
                        traceM $ "Error parsing modifiers in " ++ sourceFile
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
            }
parseEU4Scripted _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for scripted effect in " <> T.pack file)