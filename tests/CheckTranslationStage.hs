{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckTranslationStage
    ( checkTranslationStage
    , main
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.System
import Data.DList
import Test.Hspec

import Technique.Internal
import Technique.Language
import Technique.Translate

import ExampleProcedure hiding (main)

main :: IO ()
main = do
    finally (hspec checkTranslationStage) (putStrLn ".")

stubProcedure :: Step
stubProcedure = Sequence empty

testEnv :: Environment
testEnv = Environment
    { environmentVariables = singletonMap (Identifier "x") (Name "!x")
    , environmentFunctions = singletonMap (Identifier "oven") (Subroutine exampleProcedureOven stubProcedure)
    , environmentRole = Unspecified
    , environmentAccumulated = Sequence empty
    }

checkTranslationStage :: Spec
checkTranslationStage = do
    describe "Invalid code emits expected compiler failures" $ do
        it "encountering undefined symbol" $
          let
            expr = Undefined
          in do
            runTranslate testEnv (translateExpression expr)
                `shouldBe` Left EncounteredUndefined

        it "encountering unknown variable" $
          let
            expr = Variable [Identifier "y"]
          in do
            runTranslate testEnv (translateExpression expr)
                `shouldBe` Left (UseOfUnknownIdentifier (Identifier "y"))

        it "encountering unknown procedure" $
          let
            expr1 = Variable [Identifier "x"]
            expr2 = Application (Identifier "f") expr1
          in do
            runTranslate testEnv (translateExpression expr2)
                `shouldBe` Left (CallToUnknownProcedure (Identifier "f"))
