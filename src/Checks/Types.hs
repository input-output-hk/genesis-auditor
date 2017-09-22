
module Checks.Types where

import qualified System.Console.ANSI as ANSI
import Types

type CheckName = String

type Reason = String

data CheckStatus = CheckPassed
                 | CheckFailed Reason

instance Show CheckStatus where
    show CheckPassed = "Check Passed"
    show (CheckFailed reason) = "Check Failed: " ++ reason

printCheckStatus :: CheckStatus -> IO ()
printCheckStatus CheckPassed = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
  putStrLn "Check Passed"
  ANSI.setSGR [ANSI.Reset]
printCheckStatus (CheckFailed reason) = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
  putStrLn $ "Check Failed!"
  putStrLn reason
  ANSI.setSGR [ANSI.Reset]

data Check = Check { checkName :: String
                   , runCheck :: GenesisData -> Auditor CheckStatus
                   }
