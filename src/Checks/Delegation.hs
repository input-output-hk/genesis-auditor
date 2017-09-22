{-# LANGUAGE RecordWildCards #-}
module Checks.Delegation
    (
        delegationChecks
    ) where


import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Checks.Types
import Types

delegationChecks :: [Check]
delegationChecks =
    [ delegationCheckStakeholdersMatch
    , delegationCheckAddressCorrespondence
    , delegationCheckVssCorrespondence
    , delegationLengthConsistency
    ]

delegationCheckStakeholdersMatch :: Check
delegationCheckStakeholdersMatch = Check
    { checkName = "delegation-stakeholders-match"
    , runCheck = doCheckStakeholdersMatch
    }

delegationCheckAddressCorrespondence :: Check
delegationCheckAddressCorrespondence = Check
    { checkName = "delegation-address-correspondence"
    , runCheck = doCheckAddressCorrespondence
    }

delegationCheckVssCorrespondence :: Check
delegationCheckVssCorrespondence = Check
    { checkName = "delegation-vss-correspondence"
    , runCheck = doCheckVssCorrespondence
    }


delegationLengthConsistency :: Check
delegationLengthConsistency = Check
    { checkName = "delegation-length-consistency"
    , runCheck = doDelegationLengthConsistency
    }

-- | Checks that the stakeholders in the genesis data are exactly those that we expect
doCheckStakeholdersMatch :: GenesisData -> Auditor CheckStatus
doCheckStakeholdersMatch gdata = do
    CLI{..} <- ask
    expectedStakeholders <- expectedStakeholderSet
    let actualStakeholders = actualStakeholderSet gdata
    pure $ if  expectedStakeholders == actualStakeholders
        then CheckPassed
        else CheckFailed $ unlines ["Mismatch between expected and actual stakeholders"
                                   ,"  missing stakeholders: " <> show (HS.toList $ expectedStakeholders `HS.difference` actualStakeholders)
                                   ,"  unexpected stakeholders: " <> show (HS.toList $ actualStakeholders `HS.difference` expectedStakeholders)
                                   ]

-- | Checks that every stakeholder delegates, and that only stakeholders delegate
doCheckAddressCorrespondence :: GenesisData -> Auditor CheckStatus
doCheckAddressCorrespondence gdata = do
    let actualStakeholders = actualStakeholderSet gdata
    let delegationAddresses = HS.fromMap . HM.map (const ()) . gdHeavyDelegation $ gdata
    pure $ if actualStakeholders == delegationAddresses
        then CheckPassed
        else CheckFailed $ unlines [ "Mismatch between stakeholders and delegation certificates"
                                   , "  No delegation for: " <> show (HS.toList $ actualStakeholders `HS.difference` delegationAddresses)
                                   , "  Unexpected delegation: " <> show (HS.toList $ delegationAddresses `HS.difference` actualStakeholders)
                                   ]

-- | Checks that every heavy delegate issuerPk has an entry inside vssCert. This needs to be bijective.
doCheckVssCorrespondence :: GenesisData -> Auditor CheckStatus
doCheckVssCorrespondence gdata = do
    let delegatePks = HS.fromList . HM.elems $ dc_delegatePk <$> gdHeavyDelegation gdata
    let signingKeys = HS.fromList . HM.elems $ vss_signingKey <$> gdVssCerts gdata
    pure $ if delegatePks == signingKeys
        then CheckPassed
        else CheckFailed $ unlines [ "Mismatch between delegate public keys and signing keys in vssCertificates"
                                   , "  No signing keyfor: " <> show (HS.toList $ delegatePks `HS.difference` signingKeys)
                                   , "  Unexpected signing keys: " <> show (HS.toList $ signingKeys `HS.difference` delegatePks)
                                   ]

-- | Check that the number of stakeholder addresses and vss certificates is the same.
--
-- In particular, this test fails if more than one stakeholder
-- delegate to the same core node
doDelegationLengthConsistency :: GenesisData -> Auditor CheckStatus
doDelegationLengthConsistency GenesisData{..} =
    pure $ if HM.size gdBootStakeholders == HM.size gdVssCerts
           then CheckPassed
           else CheckFailed $ "Difference in the number of stakeholders and vss certificates. "
                <> show (HM.size gdBootStakeholders)
                <> " stakeholders, but "
                <> show (HM.size gdVssCerts)

-- | The set of stakeholders that we expect to be in the genesis data
expectedStakeholderSet :: Auditor (HS.HashSet T.Text)
expectedStakeholderSet = do
    CLI{..} <- ask
    stakeholders <- T.lines <$> liftIO (T.readFile stakeholdersFile)
    pure . HS.fromList $ stakeholders

-- | The set of stakeholders in the genesis data
actualStakeholderSet :: GenesisData -> HS.HashSet T.Text
actualStakeholderSet gdata = HS.fromMap . HM.map (const ()) $ gdBootStakeholders gdata
