{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Checks.CanonicalJSON
  ( canonicalJsonCheck
  ) where

import Data.String.Conv
import Text.JSON.Canonical
import qualified Data.Aeson as Aeson
import Data.Maybe
import Control.Monad.Fail (MonadFail)
import Data.HashMap.Lazy as HM
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Checks.Types
import Types
import Control.Monad.Reader
import Control.Monad.Identity
import Crypto.Hash as Crypto

-- | Checks this is a valid canonical JSON and that the input hash corresponds.
canonicalJsonCheck :: Check
canonicalJsonCheck = Check {
    checkName = "is-canonical-json-check"
  , runCheck  = doCheck
  }

doCheck :: GenesisData -> Auditor CheckStatus
doCheck gData = do
  CLI{..} <- ask
  -- Parse the raw JSON
  canonicalJsonE <- parseCanonicalJSON . toS <$> liftIO (BS.readFile genesisFile)
  case canonicalJsonE of
    Left e   -> pure $ CheckFailed (show e)
    Right cj ->  do
      let canonicalHash = Crypto.hashWith @BS.ByteString Blake2b_256 (toS $ renderCanonicalJSON cj)
      -- Second roundtrip
      let rawBytes   = renderCanonicalJSON . runIdentity . toJSON $ gData
      let actualHash = Crypto.hashWith @BS.ByteString Blake2b_256 (toS rawBytes)
      return $ case C8.unpack expectedHash == show canonicalHash && C8.unpack expectedHash == show actualHash of
        True  -> CheckPassed
        False -> CheckFailed $ "Expecting Hash " <> show expectedHash <> " but found hash " <> show canonicalHash

instance Monad m => ToJSON m GenesisData where
    toJSON GenesisData {..} =
        mkObject
            [ ("bootStakeholders", toJSON gdBootStakeholders)
            , ("heavyDelegation", toJSON gdHeavyDelegation)
            , ("startTime", toJSON gdStartTime)
            , ("vssCerts", toJSON gdVssCerts)
            , ("nonAvvmBalances", toJSON gdNonAvvmBalances)
            , ("blockVersionData", toJSON gdBlockVersionData)
            , ("protocolConsts", toJSON gdProtocolConsts)
            , ("avvmDistr", toJSON gdAvvmDistr)
            , ("ftsSeed", toJSON gdFtsSeed)
            ]

instance (Monad m, Applicative m, MonadFail m) => ReportSchemaErrors m where
    expected expec got = fail $ mconcat
        [ "expected "
        , expec
        , " but got "
        , fromMaybe "" got
        ]

instance Monad m => ToJSON m Integer where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Aeson.Object where
    toJSON o = toJSON o

instance (ReportSchemaErrors m, Eq k, Hashable k, FromObjectKey m k, FromJSON m a) =>
         FromJSON m (HashMap k a) where
    fromJSON enc = do
        obj <- fromJSObject enc
        HM.fromList . catMaybes <$> mapM aux obj
      where
        aux (k, a) = knownKeys <$> fromObjectKey k <*> fromJSON a
        knownKeys :: Maybe k -> a -> Maybe (k, a)
        knownKeys Nothing _  = Nothing
        knownKeys (Just k) a = Just (k, a)

instance (ReportSchemaErrors m) => FromJSON m GenesisData where
    fromJSON obj = do
        gdBootStakeholders <- fromJSField obj "bootStakeholders"
        gdHeavyDelegation <- fromJSField obj "heavyDelegation"
        gdStartTime <- fromJSField obj "startTime"
        gdVssCerts <- fromJSField obj "vssCerts"
        gdNonAvvmBalances <- fromJSField obj "nonAvvmBalances"
        gdBlockVersionData <- fromJSField obj "blockVersionData"
        gdProtocolConsts <- fromJSField obj "protocolConsts"
        gdAvvmDistr <- fromJSField obj "avvmDistr"
        gdFtsSeed <- fromJSField obj "ftsSeed"
        return GenesisData {..}
