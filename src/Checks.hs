{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Checks (
  module Exports
  , performChecks
  ) where

import Checks.CanonicalJSON as Exports
import Checks.Types as Exports
import Control.Monad
import Types

performChecks :: GenesisData -> Auditor [(CheckName, CheckStatus)]
performChecks genData = mapM doCheck [
  canonicalJsonCheck
  ]
  where
    doCheck :: Check -> Auditor (CheckName, CheckStatus)
    doCheck Check{..} = (checkName,) <$> runCheck genData
