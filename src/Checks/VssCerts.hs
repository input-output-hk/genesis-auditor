{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Checks.VssCerts
  ( vssCerts
  ) where

import Data.Monoid
import Checks.Types
import Types
import Control.Monad.Reader
import Control.Monad.Identity

vssCerts :: Check
vssCerts = Check {
    checkName = "vss-certs-conformity"
  , runCheck  = doCheck
  }

doCheck :: GenesisData -> Auditor CheckStatus
doCheck gData = return . CheckFailed $ "TODO"
