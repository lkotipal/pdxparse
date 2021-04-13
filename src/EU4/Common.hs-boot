module EU4.Common (
        pp_script
    ,   ppMany
    ,   ppOne
    ,   extractStmt
    ,   matchLhsText
    ) where

import Abstract (GenericScript, GenericStatement)
import Doc (Doc)
import Messages (IndentedMessages, StatementHandler)
import SettingsTypes (PPT)
import EU4.Types (EU4Info)
import Data.Text (Text)

pp_script :: (EU4Info g, Monad m) => GenericScript -> PPT g m Doc
ppMany :: (EU4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppOne :: (EU4Info g, Monad m) => StatementHandler g m
extractStmt :: (a -> Bool) -> [a] -> (Maybe a, [a])
matchLhsText :: Text -> GenericStatement -> Bool
