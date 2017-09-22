
module Checks.Types where


import Types

type CheckName = String
type Reason = String
data CheckStatus = CheckPassed
                 | CheckFailed Reason
instance Show CheckStatus where
    show CheckPassed = "Check Passed"
    show (CheckFailed reason) = "Check Failed: " ++ reason

data Check = Check { checkName :: String
                   , runCheck :: GenesisData -> Auditor CheckStatus
                   }
