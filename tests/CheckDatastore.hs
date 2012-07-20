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
-- import qualified Data.ByteString.Char8 as S
import Test.HUnit
import Test.Hspec (Spec, describe, it)

import Lookup (lookupResource, storeResource)


spec :: Spec
spec =
    describe "Redis backend" $ do
        testStoreKey
        testReadKey

answer' :: ByteString
answer' = "Life, universe, and everything"

testStoreKey =
    it "stores a key" $ do
        storeResource k v'
        assertBool "" True
  where
    k  = 42
    v' = answer'
    
testReadKey =
    it "reads a key" $ do
        res' <- lookupResource k
        assertEqual "wrong value returned" v' res'
  where
    k  = 42
    v' = answer'
