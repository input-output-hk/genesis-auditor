
module Types where

import Data.Aeson as JSON

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
    { gdBootStakeholders :: GenesisWStakeholders
    , gdHeavyDelegation  :: GenesisDelegation
    , gdStartTime        :: Timestamp
    , gdVssCerts         :: GenesisVssCertificatesMap
    , gdNonAvvmBalances  :: GenesisNonAvvmBalances
    , gdBlockVersionData :: BlockVersionData
    , gdProtocolConsts   :: ProtocolConstants
    , gdAvvmDistr        :: GenesisAvvmBalances
    , gdFtsSeed          :: SharedSeed
    } deriving (Show, Eq)
