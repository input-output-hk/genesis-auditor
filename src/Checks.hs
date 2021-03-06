{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Checks (
  module Exports
  , performChecks
  ) where

import Checks.Balance as Exports
import Checks.CanonicalJSON as Exports
import Checks.Delegation as Exports
import Checks.VssCerts as Exports
import Checks.DuplicateKeys as Exports
import Checks.Types as Exports
import Types

-- | Runs all the self-contained checks, reporting the final outcome.
performChecks :: GenesisData -> Auditor [(CheckName, CheckStatus)]
performChecks genData = mapM doCheck $
                        canonicalJsonChecks
                        ++ [duplicateKeysCheck]
                        ++ vssCertsCheck
                        ++ delegationChecks
                        ++ balanceChecks
  where
    doCheck :: Check -> Auditor (CheckName, CheckStatus)
    doCheck Check{..} = (checkName,) <$> runCheck genData
