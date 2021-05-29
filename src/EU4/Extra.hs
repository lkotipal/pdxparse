{-|
Module      : EU4.Extra
Description : Handles for extra files (specified on the command line)
-}
module EU4.Extra (writeEU4Extra) where

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
import EU4.Common
import EU4.Handlers (plainMsg)
import FileIO (Feature (..), writeFeatures)
import Messages (imsg2doc, IndentedMessages)
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions
                     , indentUp )

writeEU4Extra :: (EU4Info g, MonadIO m) => PPT g m ()
writeEU4Extra = do
    scripts <- getExtraScripts
    let features :: [Feature GenericScript]
        features = map (\(f, s) -> Feature { featurePath = Just f
                                           , featureId = Just $ T.pack (takeFileName f)
                                           , theFeature = Right s })
                       (HM.toList scripts)
    writeFeatures "extra" features pp_extra

pp_extra :: (EU4Info g, Monad m) => GenericScript -> PPT g m Doc
pp_extra scr = imsg2doc =<< ppExtra scr

ppExtra :: (EU4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppExtra scr = indentUp (concat <$> mapM ppExtraLine scr)

ppExtraLine :: (EU4Info g, Monad m) => GenericStatement -> PPT g m IndentedMessages
ppExtraLine stmt@[pdx| $lhs = @scr |] = do
    loc <- getGameL10nIfPresent lhs
    let label = case loc of
                    Just t -> t <> "<!-- " <> lhs <> " -->"
                    _ -> lhs
    [labMsg] <- plainMsg (label <> ":")
    msgs <- ppMany scr
    return (labMsg : msgs)
ppExtraLine stmt = ppOne stmt
