{-# LANGUAGE TypeApplications #-}
module CLI (parseCLI) where

import Options.Applicative
import Data.ByteString.Char8 as C8
import Data.String.Conv
import Data.Monoid
import qualified Data.Text as T
import Types
import Crypto.Hash

parseCLI :: Parser CLI
parseCLI = CLI
      <$> parserHash
      <*> parseStakeholdersFile
      <*> parseVssFile
      <*> parseGenesisFile

parserHash :: Parser ByteString
parserHash = C8.pack <$> strOption
          ( long "target-sha"
         <> metavar "BLAKE256-SHA"
         <> help "Expected SHA for the canonical JSON encoding." )
  where
    readDigest :: String -> Either String (Digest Blake2b_256)
    readDigest x = case digestFromByteString (toS x :: ByteString) of
      Nothing -> Left $ "Invalid Digest: " <> x
      Just d -> Right d

parseStakeholdersFile :: Parser FilePath
parseStakeholdersFile = strOption
          ( short 's'
         <> metavar "PATH-TO-STAKEHOLDERS-FILE"
         <> help "The path to a file containing the expected stakeholders, one per line." )

parseVssFile :: Parser FilePath
parseVssFile = strOption
          ( short 'c'
         <> metavar "PATH-TO-VSS-CERTS-FILE"
         <> help "The path to a file containing the expected vss certificates, one per line." )

parseGenesisFile :: Parser FilePath
parseGenesisFile = strOption
          ( short 'i'
         <> metavar "PATH-TO-INPUT-GENESIS-JSON"
         <> help "The path to the input genesis JSON file." )
