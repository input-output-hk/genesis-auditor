{-# LANGUAGE RecordWildCards #-}
module Lib where

import Types
import Data.String.Conv
import qualified Data.ByteString as BS
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import Control.Monad.Reader

readGenesisFile :: Auditor ByteString
readGenesisFile = do
  CLI{..} <- ask
  liftIO $ BS.readFile genesisFile

parseGenesisFile :: ByteString -> Either String GenesisData
parseGenesisFile = JSON.eitherDecode . toS
