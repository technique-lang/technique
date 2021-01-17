{-# LANGUAGE OverloadedStrings #-}

module MockData (spec) where

import Lookup (flushDatastore, storeResource)
import Test.Hspec (Spec, describe, it)
import Utilities (assertPass)

spec :: Spec
spec = do
    describe "Load mock data for tests" $ do
        testFlushDatastore
        testSetFakeData

testFlushDatastore =
    it "ensure clean slate" $ do
        flushDatastore

testSetFakeData =
    it "store mock data" $ do
        storeResource "254" "{\"president\": \"Kennedy\"}\n"
        storeResource "42:config" "[null]"
        assertPass
