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
      <*> parseAvvmFile
      <*> parseGenesisFile
      <*> parseRemovedFromAvvm
      <*> parseAddedToAvvm
      <*> parseVerbosity

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

parseAvvmFile :: Parser FilePath
parseAvvmFile = strOption
          ( short 'a'
         <> metavar "PATH-TO-AVVM-OUTPUT-FILE"
         <> help "The path to a file containing the expected AVVM stake distribution (JSON)." )

parseGenesisFile :: Parser FilePath
parseGenesisFile = strOption
          ( short 'i'
         <> metavar "PATH-TO-INPUT-GENESIS-JSON"
         <> help "The path to the input genesis JSON file." )

parseRemovedFromAvvm :: Parser (Maybe FilePath)
parseRemovedFromAvvm = optional $ strOption
          ( long "removed-from-avvm"
         <> metavar "REMOVED-FROM-AVVM-JSON"
         <> help "Contains entries that are known to be removed from the AVVM output." )

parseAddedToAvvm :: Parser (Maybe FilePath)
parseAddedToAvvm = optional $ strOption
          ( long "added-to-avvm"
         <> metavar "ADDED-TO-AVVM-JSON"
         <> help "Contains entries that are known to be added to the AVVM output." )

parseVerbosity :: Parser Bool
parseVerbosity = switch
          ( short 'v'
         <> help "Use this flag to have more verbose output." )
