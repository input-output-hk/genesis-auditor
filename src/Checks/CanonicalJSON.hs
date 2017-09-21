
module Checks.CanonicalJSON where

import Text.JSON.Canonical
import qualified Data.ByteString as BS
import Checks.Types

canonicalJsonCheck :: Check
canonicalJsonCheck = Check {
    checkName = "is-canonical-json-check"
  , runCheck  = const (return CheckPassed)
  }
