module Main where

import Lib
import CLI
import Data.Monoid
import Options.Applicative
import Control.Monad
import Types
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Checks

main :: IO ()
main = do
  cli <- execParser opts
  flip runReaderT cli $ runAuditor $ do
    res <- parseGenesisFile <$> readGenesisFile
    case res of
      Left e -> error e
      Right genData -> performChecks genData >>= renderChecks
  where
    opts = info (parseCLI <**> helper)
      ( fullDesc
     <> progDesc "Genesis auditor tool."
     <> header "genesis-auditor" )

renderChecks :: [(CheckName, CheckStatus)] -> Auditor ()
renderChecks allChecks = forM_ allChecks $ \(cn, status) -> liftIO $ do
  putStr $ cn <> " -> "
  putStrLn (show status)
