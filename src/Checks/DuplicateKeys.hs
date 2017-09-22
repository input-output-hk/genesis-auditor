{-# LANGUAGE RecordWildCards #-}
module Checks.DuplicateKeys
    (
        duplicateKeysCheck
    ) where


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

duplicateKeysCheck :: Check
duplicateKeysCheck = Check
    { checkName = "duplicate-keys-check"
    , runCheck = doCheckDuplicateKeys
    }

-- http://teh.id.au/posts/2017/03/13/accumulating-errors/
hoistErrors :: Either e a -> Errors e a
hoistErrors e =
  case e of
    Left es ->
      Other (Constant es)
    Right a ->
      Pure a

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
      -- We need to check four main entries:
      -- 1. avvmDistr
      -- 2. bootStakeholders
      -- 3. heavyDelegation
      -- 4. vssCerts
      let finalResult = collectErrors [checkAvvmDistr jsonBlob, checkStakeholders jsonBlob, checkDelegation jsonBlob, checkVss jsonBlob]
      pure $ case finalResult of
        Right    _ -> CheckPassed
        Left  errs -> CheckFailed (show errs)

withObject :: String -> (JSON.JSValue -> Either [CheckStatus] ()) -> JSON.JSValue -> Either [CheckStatus] ()
withObject key f (JSON.JSObject assocMap) = case List.lookup key (JSON.fromJSObject assocMap) of
  Nothing -> Left [CheckFailed $ "key " ++ key ++ " not found in JSON input."]
  Just v  -> f v
withObject _ _ x = Left [CheckFailed $ " withObject called not on a JSON Object: " ++ show x]

checkDupes :: String -> JSON.JSValue -> Either [CheckStatus] ()
checkDupes label (JSON.JSObject assocMap) =
  let keys = map fst (JSON.fromJSObject assocMap)
  in case filter ((> 1) . length) (List.group . List.sort $ keys) of
       []    -> Right ()
       dupes -> Left [ CheckFailed $ "[" ++ label ++ "] Duplicates found:" ++ show (map (take 1) dupes)
                     ]
checkDupes _ x = Left [CheckFailed $ "Not a JSON Object: " ++ show x]

--
-- The actual checkers.
--

checkAvvmDistr :: JSON.JSValue -> Either [CheckStatus] ()
checkAvvmDistr = withObject "avvmDistr" (checkDupes "avvmDistr")

checkStakeholders :: JSON.JSValue -> Either [CheckStatus] ()
checkStakeholders  = withObject "bootStakeholders" (checkDupes "bootStakeholders")

checkDelegation :: JSON.JSValue -> Either [CheckStatus] ()
checkDelegation = withObject "heavyDelegation" (checkDupes "heavyDelegation")

checkVss :: JSON.JSValue -> Either [CheckStatus] ()
checkVss = withObject "vssCerts" (checkDupes "vssCerts")
