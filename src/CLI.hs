
module CLI (parseCLI) where

import Options.Applicative
import Data.ByteString.Char8 as C8
import Data.Monoid
import Types

parseCLI :: Parser CLI
parseCLI = CLI
      <$> parserHash
      <*> parseGenesisFile

parserHash :: Parser Hash
parserHash = Hash . C8.pack <$> strOption
          ( long "target-sha"
         <> metavar "BLAKE256-SHA"
         <> help "Expected SHA for the canonical JSON encoding." )

parseGenesisFile :: Parser FilePath
parseGenesisFile = strOption
          ( short 'i'
         <> metavar "PATH-TO-INPUT-GENESIS-JSON"
         <> help "The path to the input genesis JSON file." )
