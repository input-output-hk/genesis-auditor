{-# LANGUAGE RecordWildCards #-}
module Checks.Delegation
    (
        delegationChecks
    ) where


import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Monoid
import Data.Text (Text)

import Checks.Types
import Types

delegationChecks :: [Check]
delegationChecks =
    [ delegationCheckStakeholdersMatch
    ]

delegationCheckStakeholdersMatch :: Check
delegationCheckStakeholdersMatch = Check
    { checkName = "delegation-stakeholders-match"
    , runCheck = doCheckStakeholdersMatch
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

-- | The set of stakeholders that we expect to be in the genesis data
expectedStakeholderSet :: Auditor (HS.HashSet Text)
expectedStakeholderSet = do
    CLI{..} <- ask
    pure . HS.fromList $ stakeholders

-- | The set of stakeholders in the genesis data
actualStakeholderSet :: GenesisData -> HS.HashSet Text
actualStakeholderSet gdata = HS.fromMap . HM.map (const ()) $gdBootStakeholders gdata
