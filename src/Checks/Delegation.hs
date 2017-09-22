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

-- | Checks that the stakeholders in the genesis data are exactly those that we expect
doCheckStakeholdersMatch :: GenesisData -> Auditor CheckStatus
doCheckStakeholdersMatch gdata = do
    CLI{..} <- ask
    expectedStakeholders <- expectedStakeholderSet
    let actualStakeholders = actualStakeholderSet gdata
    pure $ if  expectedStakeholders == actualStakeholders
        then CheckPassed
        else CheckFailed $ unlines ["Expecting stakeholders " <> show expectedStakeholders <> " but got " <> show actualStakeholders
                                   ,"  missing stakeholders: " <> show (expectedStakeholders `HS.difference` actualStakeholders)
                                   ,"  unexpected stakeholders: " <> show (actualStakeholders `HS.difference` expectedStakeholders)
                                   ]

-- | Checks that every stakeholder delegates, and that only stakeholders delegate
doCheckAddressCorrespondence :: GenesisData -> Auditor CheckStatus
doCheckAddressCorrespondence gdata = do
    let actualStakeholders = actualStakeholderSet gdata
    let delegationAddresses = HS.fromMap . HM.map (const ()) . gdHeavyDelegation $ gdata
    pure $ if actualStakeholders == delegationAddresses
        then CheckPassed
        else CheckFailed $ unlines [ "Mismatch between stakeholders and delegation certificates"
                                   , "  No delegation for: " <> show (actualStakeholders `HS.difference` delegationAddresses)
                                   , "  Unexpected delegation: " <> show (delegationAddresses `HS.difference` actualStakeholders)
                                   ]

-- | The set of stakeholders that we expect to be in the genesis data
expectedStakeholderSet :: Auditor (HS.HashSet T.Text)
expectedStakeholderSet = do
    CLI{..} <- ask
    stakeholders <- T.lines <$> liftIO (T.readFile stakeholdersFile)
    pure . HS.fromList $ stakeholders

-- | The set of stakeholders in the genesis data
actualStakeholderSet :: GenesisData -> HS.HashSet T.Text
actualStakeholderSet gdata = HS.fromMap . HM.map (const ()) $ gdBootStakeholders gdata
