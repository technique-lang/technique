{-# LANGUAGE OverloadedStrings #-}

module CheckDatastore (spec) where

import Prelude hiding (catch)

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, it)
import Test.HUnit

import Lookup (lookupResource, storeResource)
import Utilities (assertMaybe)


spec :: Spec
spec = do
    describe "Redis backend" $ do
        testStoreKey
        testReadKey
        testNonexistentKey


answer' :: ByteString
answer' = "Life, universe, and everything"

testStoreKey =
    it "stores a value at a given key" $ do
        storeResource k' v'
        assertBool "" True      -- it didn't throw an exception; good
  where
    k' = "42"
    v' = answer'

testReadKey =
    it "reads that key and gets the same value back" $ do
        res'0 <- lookupResource k
        assertMaybe "no value for this key" res'0
        let res' = fromJust res'0
        assertEqual "wrong value returned" v' res'
  where
    k  = "42"
    v' = answer'

testNonexistentKey =
    it "handles non-existent keys" $ do
        res'0 <- lookupResource k
        case res'0 of
            Just _  -> assertFailure "Non-existent key; should not have a value!"
            Nothing -> return ()
  where
    k  = "a9s1t$y9e"

