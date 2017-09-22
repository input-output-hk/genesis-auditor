{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Checks.VssCerts
  ( vssCertsCheck
  ) where

import Data.Monoid
import Checks.Types
import Types
import Data.String.Conv
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Text.Pretty.Simple as PP

vssCertsCheck :: [Check]
vssCertsCheck = [
  vssConfirmityCheck
  ]

vssConfirmityCheck :: Check
vssConfirmityCheck = Check {
    checkName = "vss-certs-conformity"
  , runCheck  = doCheck
  }

doCheck :: GenesisData -> Auditor CheckStatus
doCheck gData = do
  -- Check that what we read as input from the JSON file
  -- is the same we expect in the certs file.
  CLI{..} <- ask
  expectedCerts <- expectedCertsSet
  let actualCerts = actualCertsSet gData
  pure $ if  expectedCerts == actualCerts
      then CheckPassed
      else CheckFailed $ unlines ["Mismatch between expected and actual VSS certs"
                                 ,"  missing certs: " <> toS (PP.pShowNoColor (HS.toList $ expectedCerts `HS.difference` actualCerts))
                                 ,"  unexpected certs: " <> toS (PP.pShowNoColor (HS.toList $ actualCerts `HS.difference` expectedCerts))
                                 ]

-- | The set of certs that we expect to be in the genesis data
expectedCertsSet :: Auditor (HS.HashSet T.Text)
expectedCertsSet = do
    CLI{..} <- ask
    certs <- T.lines <$> liftIO (T.readFile vssCertsFile)
    pure . HS.fromList $ certs

-- | The set of stakeholders in the genesis data
actualCertsSet :: GenesisData -> HS.HashSet T.Text
actualCertsSet gdata = HS.fromMap . HM.map (const ()) $ gdVssCerts gdata
