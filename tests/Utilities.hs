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

module Utilities (
    assertMaybe,
    assertPass
) where

import Test.HUnit

--
-- Seems a glaring omission from HUnit; assert that a Maybe is not Nothing.
--

assertMaybe :: String -> Maybe a -> Assertion
assertMaybe prefix m0 =
    case m0 of
        Nothing -> assertFailure prefix
        Just _  -> assertBool "" True

--
-- For when code didn't throw an exception; puts it in Assertion which is IO ().
--
assertPass :: Assertion
assertPass = do
    assertBool "" True

