{-|
Module      : HOI4.Extra
Description : Handles for extra files (specified on the command line)
-}
module HOI4.Extra (writeHOI4Extra, writeHOI4ExtraCountryScope, writeHOI4ExtraProvinceScope, writeHOI4ExtraModifier) where

import Debug.Trace (trace)
import Control.Monad.Trans (MonadIO (..))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Monoid ((<>))

import System.FilePath (takeFileName)

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import HOI4.Common
import HOI4.Handlers (plainMsg')
import FileIO (Feature (..), writeFeatures)
import HOI4.Messages (imsg2doc, IndentedMessages)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions
                     , indentUp )

writeHOI4Extra :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Extra = do
    scripts <- getExtraScripts
    let features :: [Feature GenericScript]
        features = map (\(f, s) -> Feature { featurePath = Just f
                                           , featureId = Just $ T.pack (takeFileName f)
                                           , theFeature = Right s })
                       (HM.toList scripts)
    writeFeatures "extra" features pp_extra

writeHOI4ExtraCountryScope :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4ExtraCountryScope = do
    scripts <- getExtraScriptsCountryScope
    let features :: [Feature GenericScript]
        features = map (\(f, s) -> Feature { featurePath = Just f
                                           , featureId = Just $ T.pack (takeFileName f)
                                           , theFeature = Right s })
                       (HM.toList scripts)
    writeFeatures "extraCountryScope" features (\e -> scope (HOI4Country) $ pp_extra_without_label e)

writeHOI4ExtraProvinceScope :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4ExtraProvinceScope = do
    scripts <- getExtraScriptsProvinceScope
    let features :: [Feature GenericScript]
        features = map (\(f, s) -> Feature { featurePath = Just f
                                           , featureId = Just $ T.pack (takeFileName f)
                                           , theFeature = Right s })
                       (HM.toList scripts)
    writeFeatures "extraProvinceScope" features (\e -> scope (HOI4ScopeState) $ pp_extra_without_label e)

writeHOI4ExtraModifier :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4ExtraModifier = do
    scripts <- getExtraScriptsModifier
    let features :: [Feature GenericScript]
        features = map (\(f, s) -> Feature { featurePath = Just f
                                           , featureId = Just $ T.pack (takeFileName f)
                                           , theFeature = Right s })
                       (HM.toList scripts)
    writeFeatures "extraModifier" features (\e -> scope (HOI4Bonus) $ pp_extra_without_label e)

pp_extra_without_label :: (HOI4Info g, Monad m) => GenericScript -> PPT g m Doc
pp_extra_without_label scr = imsg2doc =<< ppExtraWithoutLabel scr

ppExtraWithoutLabel :: (HOI4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppExtraWithoutLabel scr = indentUp (concat <$> mapM ppOne scr)

pp_extra :: (HOI4Info g, Monad m) => GenericScript -> PPT g m Doc
pp_extra scr = imsg2doc =<< ppExtra scr

ppExtra :: (HOI4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppExtra scr = indentUp (concat <$> mapM ppExtraLine scr)

ppExtraLine :: (HOI4Info g, Monad m) => GenericStatement -> PPT g m IndentedMessages
ppExtraLine stmt@[pdx| $lhs = @scr |] = do
    loc <- getGameL10nIfPresent lhs
    let label = case loc of
                    Just t -> t <> "<!-- " <> lhs <> " -->"
                    _ -> lhs
    labMsg <- plainMsg' (label <> ":")
    msgs <- ppMany scr
    return (labMsg : msgs)
ppExtraLine stmt = ppOne stmt
