{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckEvaluationPhase
    ( checkEvaluationPhase
    , main
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.System
import Data.DList (fromList)
import Data.Int (Int64)
import Test.Hspec

import Technique.Internal
import Technique.Evaluator
import Technique.Quantity

import ExampleProcedure hiding (main)

main :: IO ()
main = do
    finally (hspec checkEvaluationPhase) (putStrLn ".")

testContext :: Context
testContext = emptyContext


exampleSubroutineOven :: Function
exampleSubroutineOven = Subroutine exampleProcedureOven NoOp

exampleSequence :: Step
exampleSequence = Nested 0 (fromList
    [ undefined
    ])

singleStep :: Step
singleStep = Known 0 (Quanticle (Number 42))


checkEvaluationPhase :: Spec
checkEvaluationPhase = do
    describe "Evaluator behaviour fundamentals" $ do
        it "single Step of Known Quantity evaluates" $ do
            result <- runEvaluate testContext (evaluateStep singleStep)
            result `shouldBe` Quanticle (Number 42)