{-
Module      : HOI4.Misc
Description : Feature handler for miscellaneous features in Hearts of Iron IV
-}
module HOI4.Misc (
        parseHOI4CountryHistory
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Char (toLower)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))

import System.FilePath (takeFileName)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
import HOI4.Messages -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import HOI4.Common -- everything

newHOI4CountryHistory :: Text -> HOI4CountryHistory
newHOI4CountryHistory chtag = HOI4CountryHistory chtag undefined

parseHOI4CountryHistory :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text HOI4CountryHistory)
parseHOI4CountryHistory scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr -> setCurrentFile sourceFile $ mapM processPolitics $ concatMap mapHisto scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing country history: " ++ T.unpack err
            return HM.empty
        Right countryHistoryFilesOrErrors ->
            flip HM.traverseWithKey countryHistoryFilesOrErrors $ \sourceFile ecohi ->
                fmap (mkCHMap . catMaybes) . forM ecohi $ \case
                    Left err -> do
                        traceM $ "Error parsing modifiers in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right cchis -> return cchis
    where
        mkCHMap :: [HOI4CountryHistory] -> HashMap Text HOI4CountryHistory
        mkCHMap = HM.fromList . map (chTag &&& id)

        mapHisto scr = case scr of
            [pdx| set_politics = @pol |] -> pol
            _ -> []

processPolitics :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe HOI4CountryHistory))
processPolitics (StatementBare _) = throwError "bare statement at top level"
processPolitics [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            let chtag = T.pack $ take 3 $ takeFileName file
            mmod <- hoistErrors $ foldM processPoliticsAddSection
                                        (Just (newHOI4CountryHistory chtag))
                                        parts
            case mmod of
                Left err -> return (Left err)
                Right Nothing -> return (Right Nothing)
                Right (Just mod) -> withCurrentFile $ \file ->
                    return (Right (Just mod ))
        _ -> throwError "unrecognized form for set_politics"
    _ -> throwError "unrecognized form for set_politics"
processPolitics _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for set_politics in " <> T.pack file)

processPoliticsAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe HOI4CountryHistory -> GenericStatement -> PPT g m (Maybe HOI4CountryHistory)
processPoliticsAddSection _ _ = return Nothing
processPoliticsrAddSection cohi stmt
    = sequence (processPoliticsAddSection' <$> cohi <*> pure stmt)
    where
        processPoliticsAddSection' cohi stmt@[pdx| ruling_party = $id |] =
            withCurrentFile $ \file -> do
                let tag = take 3 $ takeFileName file
                return cohi { chTag = T.pack tag
                     , chRulingTag = T.pack (concat[tag , "_" , T.unpack id])}
        processPoliticsAddSection' cohi stmt@[pdx| last_election = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt@[pdx| election_frequency = %_ |]
            = return cohi
        processPoliticsAddSection' cohi stmt@[pdx| elections_allowed = %_ |]
            = return cohi
        processPoliticsAddSection' cohi [pdx| $other = %_ |]
            = trace ("unknown set_politics in history: " ++ T.unpack other) $ return cohi
        processPoliticsAddSection' cohi _
            = trace ("unrecognised form for set_politics in history") $ return cohi