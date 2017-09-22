{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Data.HashMap.Lazy as HM
import Data.Aeson as JSON
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Int (Int64)
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString

data CLI = CLI
  { expectedHash     :: ByteString
  -- | Path to a file containing all the stakeholders that are
  -- supposed to be mentioned (and have delegated their stake) in the
  -- genesis file
  , stakeholdersFile :: FilePath
  -- | Path to a file containing all the VSS certificates that are
  -- supposed to be mentioned in the genesis file, one per core node.
  , vssCertsFile     :: FilePath
  , avvmFile         :: FilePath
  , genesisFile      :: FilePath
  } deriving Show

newtype Auditor a = Auditor { runAuditor :: ReaderT CLI IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CLI)

data DelegationCertificate = DelegationCertificate {
    dc_cert       :: !Text
  , dc_delegatePk :: !Text
  , dc_issuerPk   :: !Text
  , dc_omega      :: !Int
  } deriving (Show, Eq)

deriveFromJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''DelegationCertificate

data VssCertificate = VssCertificate {
    vss_expiryEpoch :: !Int
  , vss_signature   :: !Text
  , vss_vssKey      :: !Text
  , vss_signingKey  :: !Text
  } deriving (Show, Eq)

deriveFromJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''VssCertificate

type GenesisWStakeholders      = JSON.Object -- TODO: check what happens to duplicates
type Timestamp                 = JSON.Value
type GenesisNonAvvmBalances    = JSON.Object
type BlockVersionData          = JSON.Value
type ProtocolConstants         = JSON.Value
type GenesisAvvmBalances       = HM.HashMap Text Text
type SharedSeed                = JSON.Value
type AddressHash               = Text
type GenesisDelegation         = HM.HashMap AddressHash DelegationCertificate
type VssCerts                  = HM.HashMap AddressHash VssCertificate

data AvvmEntry = AvvmEntry
    { ae_vendingAddress :: Text
    , ae_ada :: Int64
    } deriving (Show, Eq)

deriveFromJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''AvvmEntry

type AvvmLedger = [AvvmEntry]

data GenesisData = GenesisData
    { gdAvvmDistr        :: GenesisAvvmBalances
    , gdBlockVersionData :: BlockVersionData
    , gdBootStakeholders :: GenesisWStakeholders
    , gdFtsSeed          :: SharedSeed
    , gdHeavyDelegation  :: GenesisDelegation
    , gdNonAvvmBalances  :: GenesisNonAvvmBalances
    , gdProtocolConsts   :: ProtocolConstants
    , gdStartTime        :: Timestamp
    , gdVssCerts         :: VssCerts
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
