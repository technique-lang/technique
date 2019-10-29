{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckQuantityBehaviour
    ( checkQuantityBehaviour
    )
where

import Core.Text.Rope ()
import Core.Text.Utilities
import Core.System
import Test.Hspec

import Technique.Quantity
import Technique.Formatter

main :: IO ()
main = do
    finally (hspec checkQuantityBehaviour) (putStrLn ".")

renderTest :: Render a => a -> String
renderTest x = show (intoDocA x)

{-|
These are less tests than a body of code that exercises construction of
an abstract syntax tree.
-}
checkQuantityBehaviour :: Spec
checkQuantityBehaviour = do
    describe "Quantity Type" $ do
        it "Numbers serialze as integers" $ do
            renderTest (Number 42) `shouldBe` "42"

        it "Simple number and unit" $ do
            renderTest (Quantity 4 "kg") `shouldBe` "4 kg"

        it "Abbreviated measurement and unit" $ do
            renderTest (Measured 4 0 0 0 "kg") `shouldBe` "4 kg"

        it "Full measurement with uncertainty, magnitude, and unit" $ do
            renderTest (Measured 4 1 0 2 "kg") `shouldBe` "4 ± 1 × 10^2 kg"