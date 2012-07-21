--
-- Procedures
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made available
-- to you by its authors as open source software: you can redistribute it
-- and/or modify it under the terms of the GNU General Public License version
-- 2 ("GPL") as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
--
-- You should have received a copy of the GPL along with this program. If not,
-- see http://www.gnu.org/licenses/. The authors of this program may be
-- contacted through http://research.operationaldynamics.com/
--

{-# LANGUAGE OverloadedStrings #-}

module CheckDatastore (spec) where
 
import Prelude hiding (catch)

import Data.ByteString (ByteString)
import Test.HUnit
import Test.Hspec (Spec, describe, it)
import Data.Maybe (fromJust)

import Utilities (assertMaybe)
import Lookup (lookupResource, storeResource)


spec :: Spec
spec =
    describe "Redis backend" $ do
        testStoreKey
        testReadKey
        testSetFakeData
        testNonexistentKey

answer' :: ByteString
answer' = "Life, universe, and everything"

testStoreKey =
    it "stores a value at a given key" $ do
        storeResource k v'
        assertBool "" True      -- it didn't throw an exception; good
  where
    k  = 42
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



testSetFakeData =
  it "setup mock data" $ do
        storeResource k v'
        assertBool "" True      -- it didn't throw an exception; good
  where
    k  = 254
    v' = "{\"president\": \"\"}"
