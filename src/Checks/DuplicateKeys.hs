{-# LANGUAGE RecordWildCards #-}
module Checks.DuplicateKeys
    (
        duplicateKeysCheck
    ) where


import Data.String.Conv
import Control.Applicative.Lift
import Control.Monad.Reader
import Data.Functor.Constant
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.IO as T
import Checks.Types
import Types

import qualified Text.JSON.String as JSON
import qualified Text.JSON as JSON
import qualified Text.Pretty.Simple as PP

duplicateKeysCheck :: Check
duplicateKeysCheck = Check
    { checkName = "duplicate-keys-check"
    , runCheck = doCheckDuplicateKeys
    }

-- http://teh.id.au/posts/2017/03/13/accumulating-errors/
hoistErrors :: Either e a -> Errors e a
hoistErrors = either (Other . Constant) Pure

collectErrors :: Monoid e => [Either e ()] -> Either e ()
collectErrors lst = case (runErrors . traverse hoistErrors) lst of
  Left e  -> Left e
  Right _ -> Right ()

doCheckDuplicateKeys :: GenesisData -> Auditor CheckStatus
doCheckDuplicateKeys _ = do
  CLI{..} <- ask
  rawJson <- T.unpack <$> liftIO (T.readFile genesisFile)
  case JSON.runGetJSON JSON.readJSObject rawJson of
    Left e         -> return $ CheckFailed (show e)
    Right jsonBlob -> do
      let finalResult = collectErrors [ checkAvvmDistr jsonBlob
                                      , checkStakeholders jsonBlob
                                      , checkDelegation jsonBlob
                                      , checkVss jsonBlob
                                      ]
      pure $ case finalResult of
        Right    _ -> CheckPassed
        Left  errs -> CheckFailed (toS $ PP.pShowNoColor errs)

-- | "Focus" on a particular key on the JSON, applying a function to the value, failing is the type mismatch.
withObject :: String -> (JSON.JSValue -> Either [CheckStatus] ()) -> JSON.JSValue -> Either [CheckStatus] ()
withObject key f (JSON.JSObject assocMap) = case List.lookup key (JSON.fromJSObject assocMap) of
  Nothing -> Left [CheckFailed $ "key " ++ key ++ " not found in JSON input."]
  Just v  -> f v
withObject _ _ x = Left [CheckFailed $ " withObject called not on a JSON Object: " ++ show x]

-- | Check the dupes by sorting the entries & grouping them. In absence of dupes, we should get a list full
-- of singletons.
checkDupes :: String -> JSON.JSValue -> Either [CheckStatus] ()
checkDupes label (JSON.JSObject assocMap) =
  let keys = map fst (JSON.fromJSObject assocMap)
  in case filter ((> 1) . length) (List.group . List.sort $ keys) of
       []     -> Right ()
       dupes  -> Left [CheckFailed $ "[" ++ label ++ "] Duplicates found:" ++ show (map (take 1) dupes) ]
checkDupes _ x = Left [CheckFailed $ "Not a JSON Object: " ++ show x]

checkKey :: String -> JSON.JSValue -> Either [CheckStatus] ()
checkKey label = withObject label (checkDupes label)

--
-- The actual checkers.
--

checkAvvmDistr :: JSON.JSValue -> Either [CheckStatus] ()
checkAvvmDistr = checkKey "avvmDistr"

checkStakeholders :: JSON.JSValue -> Either [CheckStatus] ()
checkStakeholders  = checkKey "bootStakeholders"

checkDelegation :: JSON.JSValue -> Either [CheckStatus] ()
checkDelegation = checkKey "heavyDelegation"

checkVss :: JSON.JSValue -> Either [CheckStatus] ()
checkVss = checkKey "vssCerts"
