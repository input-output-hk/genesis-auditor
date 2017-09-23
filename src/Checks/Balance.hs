{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Checks.Balance
    (balanceChecks) where

import Data.List
import Data.Ord
import Data.Int (Int64)
import Control.Monad.Reader
import Checks.Types
import Data.Aeson as JSON
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Types
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Text.Pretty.Simple as PP
import Data.String.Conv

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL

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
            let avvmHashmap = HM.fromList $ (\(AvvmEntry address ada) -> (convertToBS64URL address, fromIntegral $ 1000000 * ada :: Int64)) <$> ledger
                genesisHashmap = HM.map (read . toS) gdAvvmDistr
                imbalance = HM.filter (/= 0) $ HM.intersectionWith (-) avvmHashmap genesisHashmap
            pure $ if avvmHashmap == genesisHashmap && HM.null imbalance
                then CheckPassed
                else let missing = avvmHashmap `HM.difference` genesisHashmap
                         unexpected = genesisHashmap `HM.difference` avvmHashmap
                         nMissing = HM.size missing
                         nUnexpected = HM.size unexpected
                         nImbalance = HM.size imbalance

                     in CheckFailed $ unlines [ "Mismatch in Avvm balance"
                                              , show nMissing <> " address(es) missing in the genesis data: " <> showHashmap verbose missing
                                              , show nUnexpected <> " unexpected address(es): " <> showHashmap verbose unexpected
                                              , show nImbalance <> " account(s) do not contain the right amount of Lovelace: " <> showHashmap verbose imbalance
                                              ]
  where
    showOrderedHashmap = toS . PP.pShowNoColor . reverse . sortBy (comparing snd) . HM.toList
    showTruncatedHashmap hm = (toS . PP.pShowNoColor . take 5 . HM.toList $ hm)
                              <> "\n    and " <> show (HM.size hm - 5) <> " more"
    showHashmap verbosity hm =
        if verbosity || HM.size hm <= 5
        then showOrderedHashmap hm
        else showTruncatedHashmap hm

convertToBS64URL :: Text -> Text
convertToBS64URL = toS
    . B64URL.encode
    . either (\err -> error $ "error in B64.decode: " <> err) id
    . B64.decode
    . toS
