{-# LANGUAGE OverloadedStrings #-}

module Utilities
  ( assertMaybe,
    assertPass,
  )
where

import Test.HUnit

--
-- Seems a glaring omission from HUnit; assert that a Maybe is not Nothing.
--

assertMaybe :: String -> Maybe a -> Assertion
assertMaybe prefix m0 =
  case m0 of
    Nothing -> assertFailure prefix
    Just _ -> assertBool "" True

--
-- For when code didn't throw an exception; puts it in Assertion which is IO ().
--
assertPass :: Assertion
assertPass = do
  assertBool "" True
