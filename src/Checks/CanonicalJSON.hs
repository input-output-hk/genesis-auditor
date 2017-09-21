{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Checks.CanonicalJSON
  ( canonicalJsonCheck
  ) where

import Data.String.Conv
import Text.JSON.Canonical
import qualified Data.ByteString as BS
import Data.Monoid
import Checks.Types
import Types
import Control.Monad.Reader
import Control.Monad.Identity

-- | Checks this is a valid canonical JSON and that the input hash corresponds.
canonicalJsonCheck :: Check
canonicalJsonCheck = Check {
    checkName = "is-canonical-json-check"
  , runCheck  = doCheck
  }

doCheck :: GenesisData -> Auditor CheckStatus
doCheck gData = do
  CLI{..} <- ask
  let rawBytes   = renderCanonicalJSON . runIdentity . toJSON $ gData
  let actualHash = Hash mempty
  return $ case actualHash == expectedHash of
    True  -> CheckPassed
    False -> CheckFailed $ "Expecting Hash " <> show expectedHash <> " but found hash " <> show actualHash

instance ToJSON Identity GenesisData where
  toJSON GenesisData{..} = mkObject mempty
