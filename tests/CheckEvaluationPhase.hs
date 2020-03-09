{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckEvaluationPhase
    ( checkEvaluationPhase
    , main
    )
where

import Core.Data.Structures
import Core.Program
import Core.System
import Core.Text.Rope ()
import Data.DList (fromList)
import Data.Int (Int64)
import Test.Hspec hiding (context)

import Technique.Internal
import Technique.Evaluator
import Technique.Quantity

import ExampleProcedure hiding (main)

main :: IO ()
main = do
    finally (hspec checkEvaluationPhase) (putStrLn ".")

runProgram :: Program None a -> IO a
runProgram program = do
    context <- configure "0" None blank
    subProgram context program

testUnique :: Unique
testUnique = emptyUnique


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
            result <- runProgram (runEvaluate testUnique (evaluateStep singleStep))
            result `shouldBe` Quanticle (Number 42)