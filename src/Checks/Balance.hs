module Checks.Balance
    (balanceChecks) where

import Checks.Types
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Types

balanceChecks :: [Check]
balanceChecks = [ checkNonAvvmBalanceEmpty ]

checkNonAvvmBalanceEmpty :: Check
checkNonAvvmBalanceEmpty = Check
    { checkName = "balance-non-avvm-balance-empty"
    , runCheck = doCheckNonAvvmBalanceEmpty
    }

doCheckNonAvvmBalanceEmpty :: GenesisData -> Auditor CheckStatus
doCheckNonAvvmBalanceEmpty gdata =
    pure $ case gdNonAvvmBalances gdata of
               x | HM.null x -> CheckPassed
               x ->  CheckFailed $ "Non-empty nonAvvmBalances: " <> show x
