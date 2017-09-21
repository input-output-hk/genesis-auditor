module Lib
    ( someFunc
    ) where

import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"


slurpGenesisFile :: Either String GenesisData
slurpGenesisFile = Left "error"
