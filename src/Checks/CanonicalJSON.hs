{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Checks.CanonicalJSON
  ( canonicalJsonCheck
  ) where

import Data.String.Conv
import Text.JSON.Canonical
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Checks.Types
import Types
import Control.Monad.Reader
import Control.Monad.Identity
import Crypto.Hash as Crypto

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
  let actualHash = Crypto.hashWith @BS.ByteString Blake2b_256 (toS rawBytes)
  return $ case C8.unpack expectedHash == show actualHash of
    True  -> CheckPassed
    False -> CheckFailed $ "Expecting Hash " <> show expectedHash <> " but found hash " <> show actualHash

instance ToJSON Identity GenesisData where
  toJSON GenesisData{..} = mkObject mempty
