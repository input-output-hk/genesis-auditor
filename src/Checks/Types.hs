
module Checks.Types where


import Types

type CheckName = String
type Reason = String
data CheckStatus = CheckPassed
                 | CheckFailed Reason
                 deriving Show

data Check = Check { checkName :: String
                   , runCheck :: GenesisData -> Auditor CheckStatus
                   }
