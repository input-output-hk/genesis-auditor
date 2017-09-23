{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Checks.CanonicalJSON
  ( canonicalJsonChecks
  ) where

import Data.Int (Int64)
import Data.String.Conv
import Text.JSON.Canonical
import Text.JSON.Canonical.Types
import qualified Data.Scientific as Scientific
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Fail (MonadFail)
import Data.HashMap.Lazy as HM
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import Checks.Types
import Types
import Control.Monad.Reader
import Crypto.Hash as Crypto


canonicalJsonChecks :: [Check]
canonicalJsonChecks = [
    canonicalJsonCheck
  ]

-- | Checks this is a valid canonical JSON and that the input hash corresponds.
canonicalJsonCheck :: Check
canonicalJsonCheck = Check {
    checkName = "is-canonical-json-check"
  , runCheck  = doCheck
  }

doCheck :: GenesisData -> Auditor CheckStatus
doCheck _ = do
  CLI{..} <- ask
  -- Parse the raw JSON
  canonicalJsonE <- parseCanonicalJSON . toS <$> liftIO (BS.readFile genesisFile)
  case canonicalJsonE of
    Left e   -> pure $ CheckFailed (show e)
    Right cj ->  do
      let canonicalHash = Crypto.hashWith @BS.ByteString Blake2b_256 (toS $ renderCanonicalJSON cj)
      return $ case C8.unpack expectedHash == show canonicalHash of
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

instance Monad m => ToJSON m Aeson.Value where
  toJSON (Aeson.Object o) = toJSON o
  toJSON (Aeson.Array a)  = toJSON . V.toList $ a
  toJSON (Aeson.String s) = pure $ JSString (toS s)
  toJSON (Aeson.Number n) = pure $ JSNum (fromIntegral $ Scientific.coefficient n)
  toJSON (Aeson.Bool b)   = pure $ JSBool b
  toJSON Aeson.Null       = pure JSNull

instance Monad m => ToJSON m DelegationCertificate where
  toJSON DelegationCertificate{..} = mkObject [
      ("cert", toJSON dc_cert)
    , ("delegatePk", toJSON dc_delegatePk)
    , ("issuerPk", toJSON dc_issuerPk)
    , ("omega", toJSON dc_omega)
    ]

instance Monad m => ToJSON m VssCertificate where
  toJSON VssCertificate{..} = mkObject [
      ("expiryEpoch", toJSON vss_expiryEpoch)
    , ("signature", toJSON vss_signature)
    , ("vssKey", toJSON vss_vssKey)
    , ("signingKey", toJSON vss_signingKey)
    ]

instance (Monad m, ToObjectKey m k, Ord k, ToJSON m a) => ToJSON m (HashMap k a) where
    toJSON = fmap JSObject . mapM aux . List.sortOn fst . HM.toList
      where
        aux (k, a) = (,) <$> toObjectKey k <*> toJSON a

instance Monad m => ToObjectKey m T.Text where
    toObjectKey = return . toS

instance Monad m => ToJSON m Int64 where
    toJSON = return . JSNum . fromIntegral

instance Monad m => ToJSON m T.Text where
    toJSON = return . JSString . toS

instance Monad m => FromJSON m Int64 where
    fromJSON (JSNum int54) = pure . int54ToInt64 $ int54

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

instance (ReportSchemaErrors m) => FromJSON m Aeson.Value where
  fromJSON (JSObject hm) = do
    let convertHM input = do
          x <- mapM (\(k,v) -> (toS k,) <$> fromJSON v) input
          return $ HM.fromList x
    converted <- convertHM hm
    return (Aeson.Object converted)
  fromJSON JSNull        = return Aeson.Null
  fromJSON (JSBool   b)  = return (Aeson.Bool b)
  fromJSON (JSNum int54) = return (Aeson.toJSON $ int54ToInt64 int54)
  fromJSON (JSString s)  = return (Aeson.String (toS s))
  fromJSON (JSArray  a)  = Aeson.Array . V.fromList <$> mapM fromJSON a

instance (ReportSchemaErrors m) => FromJSON m T.Text where
  fromJSON (JSString s) = return (toS s)
  fromJSON _ = fail "fromJSON T.Text: type mismatch"

instance (ReportSchemaErrors m) => FromJSON m VssCertificate where
  fromJSON o@(JSObject _) =
    VssCertificate <$> fromJSField o "expiryEpoch"
                   <*> fromJSField o "signature"
                   <*> fromJSField o "vssKey"
                   <*> fromJSField o "signingKey"
  fromJSON _ = fail "fromJSON VssCertificate: type mismatch"

instance (ReportSchemaErrors m) => FromJSON m DelegationCertificate where
  fromJSON o@(JSObject _) =
    DelegationCertificate <$> fromJSField o "cert"
                          <*> fromJSField o "delegatePk"
                          <*> fromJSField o "issuerPk"
                          <*> fromJSField o "omega"
  fromJSON _ = fail "fromJSON DelegationCertificate: type mismatch"

instance (ReportSchemaErrors m) => FromObjectKey m T.Text where
  fromObjectKey = pure . Just . toS

instance (ReportSchemaErrors m) => FromJSON m GenesisData where
    fromJSON obj = do
        gdBootStakeholders <- fromJSField obj "bootStakeholders"
        gdHeavyDelegation <- fromJSField obj "heavyDelegation"
        gdVssCerts <- fromJSField obj "vssCerts"
        gdAvvmDistr <- fromJSField obj "avvmDistr"
        gdStartTime <- fromJSField obj "startTime"
        gdNonAvvmBalances <- fromJSField obj "nonAvvmBalances"
        gdBlockVersionData <- fromJSField obj "blockVersionData"
        gdProtocolConsts <- fromJSField obj "protocolConsts"
        gdFtsSeed <- fromJSField obj "ftsSeed"
        return GenesisData {..}
