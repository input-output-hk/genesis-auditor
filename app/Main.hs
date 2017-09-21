module Main where

import Lib
import CLI
import Data.Monoid
import Options.Applicative

main :: IO ()
main = do
  cli <- execParser opts
  print cli
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Genesis auditor tool."
     <> header "genesis-auditor" )
