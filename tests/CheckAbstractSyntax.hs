{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckAbstractSyntax
    ( checkAbstractSyntax
    )
where

import Core.Text.Rope ()
import Core.Text.Utilities ()
import Test.Hspec

import Technique.Language
import Technique.Quantity
import Technique.Formatter

import ExampleProcedure hiding (main)

{-|
These are less tests than a body of code that exercises construction of
an abstract syntax tree.
-}
checkAbstractSyntax :: Spec
checkAbstractSyntax = do
    describe "Constructions matching intended language design" $ do
        it "Key builtin procedures are available" $ do
            procedureName builtinProcedureTask `shouldBe` "task"

        it "Procedure's function name is correct" $ do
            procedureName exampleRoastTurkey `shouldBe` "roast_turkey"

