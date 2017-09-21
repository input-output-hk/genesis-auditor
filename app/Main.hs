module Main where

import Lib
import CLI
import Data.Monoid
import Options.Applicative
import Types
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

main :: IO ()
main = do
  cli <- execParser opts
  flip runReaderT cli $ runAuditor $ do
    (parseGenesisFile <$> readGenesisFile) >>= liftIO . print
  where
    opts = info (parseCLI <**> helper)
      ( fullDesc
     <> progDesc "Genesis auditor tool."
     <> header "genesis-auditor" )
