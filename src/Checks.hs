{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Checks (
  module Exports
  , performChecks
  ) where

import Checks.CanonicalJSON as Exports
import Checks.VssCerts as Exports
import Checks.Types as Exports
import Types

-- | Runs all the self-contained checks, reporting the final outcome.
performChecks :: GenesisData -> Auditor [(CheckName, CheckStatus)]
performChecks genData = mapM doCheck [
    canonicalJsonCheck
  , vssCerts
  ]
  where
    doCheck :: Check -> Auditor (CheckName, CheckStatus)
    doCheck Check{..} = (checkName,) <$> runCheck genData
