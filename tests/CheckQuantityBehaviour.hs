{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module CheckQuantityBehaviour
    ( checkQuantityBehaviour,
      main,
    )
where

import Core.System
import Core.Text.Rope ()
import Core.Text.Utilities
import Technique.Formatter ()
import Technique.Quantity
import Test.Hspec

main :: IO ()
main = do
    finally (hspec checkQuantityBehaviour) (putStrLn ".")

renderTest :: Render a => a -> String
renderTest x = show (highlight x)

{- |
These are less tests than a body of code that exercises construction of
an abstract syntax tree.
-}
checkQuantityBehaviour :: Spec
checkQuantityBehaviour = do
    describe "Basic behaviour of Quantity type" $ do
        it "Numbers serialze as integers" $ do
            renderTest (Number 42) `shouldBe` "42"

        it "Abbreviated measurement and unit" $ do
            renderTest (Quantity (Decimal 4 0) (Decimal 0 0) 0 "kg") `shouldBe` "4 kg"

        it "Full measurement with uncertainty, magnitude, and unit" $ do
            renderTest (Quantity (Decimal 4 0) (Decimal 1 0) 2 "kg") `shouldBe` "4 ± 1 × 10² kg"

        it "Full measurement with decimal precision" $ do
            renderTest (Quantity (Decimal 59722 4) (Decimal 6 4) 24 "kg") `shouldBe` "5.9722 ± 0.0006 × 10²⁴ kg"
