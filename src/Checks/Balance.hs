{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Checks.Balance
    (balanceChecks) where

import Control.Monad.Reader
import Checks.Types
import Data.Aeson as JSON
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Types
import Data.Text.IO as T
import qualified Text.Pretty.Simple as PP
import Data.String.Conv

balanceChecks :: [Check]
balanceChecks = [ checkNonAvvmBalanceEmpty
                , checkAvvmDistribution ]

checkNonAvvmBalanceEmpty :: Check
checkNonAvvmBalanceEmpty = Check
    { checkName = "balance-non-avvm-balance-empty"
    , runCheck = doCheckNonAvvmBalanceEmpty
    }

checkAvvmDistribution :: Check
checkAvvmDistribution = Check
    { checkName = "balance-avvm"
    , runCheck = doCheckAvvmDistribution
    }

doCheckNonAvvmBalanceEmpty :: GenesisData -> Auditor CheckStatus
doCheckNonAvvmBalanceEmpty gdata =
    pure $ case gdNonAvvmBalances gdata of
               x | HM.null x -> CheckPassed
               x ->  CheckFailed $ "Non-empty nonAvvmBalances: " <> show x

doCheckAvvmDistribution :: GenesisData -> Auditor CheckStatus
doCheckAvvmDistribution GenesisData{..} = do
    CLI{..} <- ask
    JSON.eitherDecode . toS <$> liftIO (T.readFile avvmFile) >>= \case
        Left err -> pure $ CheckFailed $ "Could not parse AvvmBalance: " <> err
        Right ledger -> do
            let avvmHashmap = HM.fromList $ (\(AvvmEntry address ada) -> (address, toS . show $ 1000000 * ada)) <$> ledger
            pure $ if avvmHashmap == gdAvvmDistr
                then CheckPassed
                else let missing = avvmHashmap `HM.difference` gdAvvmDistr
                         unexpected = gdAvvmDistr `HM.difference` avvmHashmap
                         nMissing = HM.size missing
                         nUnexpected = HM.size unexpected

                     in CheckFailed $ unlines [ "Mismatch in Avvm balance"
                                              , show nMissing <> " Addresses missing in the genesis data: " <> (toS . PP.pShowNoColor . take 5 . HM.toList $ missing)
                                                <> "\n    and " <> show (nMissing - 5) <> " more"
                                              , show nUnexpected <> " Unexpected addresses: "  <> (toS . PP.pShowNoColor . take 2 . HM.toList $ unexpected)
                                                <> "\n    and " <> show (nUnexpected - 5) <> " more"
                                              ]
                        -- TODO: look for imbalances in addresses that exist in both
