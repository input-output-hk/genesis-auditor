{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson as JSON
import Data.Aeson.Types
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Control.Monad.Reader

newtype Hash = Hash { getHash :: ByteString } deriving Show

data CLI = CLI
  { targetHash  :: Hash
  , genesisFile :: FilePath
  } deriving Show

newtype Auditor a = Auditor { runAuditor :: ReaderT CLI IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CLI)


type GenesisWStakeholders = JSON.Value
type GenesisDelegation = JSON.Value
type Timestamp = JSON.Value
type GenesisVssCertificatesMap = JSON.Value
type GenesisNonAvvmBalances = JSON.Value
type BlockVersionData = JSON.Value
type ProtocolConstants = JSON.Value
type GenesisAvvmBalances = JSON.Value
type SharedSeed = JSON.Value


data GenesisData = GenesisData
    { gdAvvmDistr        :: GenesisAvvmBalances
    , gdBlockVersionData :: BlockVersionData
    , gdBootStakeholders :: GenesisWStakeholders
    , gdFtsSeed          :: SharedSeed
    , gdHeavyDelegation  :: GenesisDelegation
    , gdNonAvvmBalances  :: GenesisNonAvvmBalances
    , gdProtocolConsts   :: ProtocolConstants
    , gdStartTime        :: Timestamp
    , gdVssCerts         :: GenesisVssCertificatesMap
    } deriving (Show, Eq)

instance FromJSON GenesisData where
  parseJSON (Object o) =
    GenesisData <$> o .: "avvmDistr"
                <*> o .: "blockVersionData"
                <*> o .: "bootStakeholders"
                <*> o .: "ftsSeed"
                <*> o .: "heavyDelegation"
                <*> o .: "nonAvvmBalances"
                <*> o .: "protocolConsts"
                <*> o .: "startTime"
                <*> o .: "vssCerts"
  parseJSON invalid = typeMismatch  "GenesisData" invalid
