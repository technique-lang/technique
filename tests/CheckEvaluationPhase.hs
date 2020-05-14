{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module CheckEvaluationPhase
  ( checkEvaluationPhase,
    main,
  )
where

import Core.Data.Structures
import Core.Program
import Core.System
import Core.Text.Rope ()
import Data.DList (fromList)
import Data.Int (Int64)
import ExampleProcedure hiding (main)
import Technique.Evaluator
import Technique.Internal
import Technique.Quantity
import Test.Hspec hiding (context)

main :: IO ()
main = do
  finally (hspec checkEvaluationPhase) (putStrLn ".")

runProgram :: Program None a -> IO a
runProgram program = do
  context <- configure "0" None blank
  subProgram context program

testEvaluation :: Evaluate Value -> IO Value
testEvaluation = runProgram . runEvaluate testUnique

testUnique :: Unique
testUnique = emptyUnique

exampleSubroutineOven :: Function
exampleSubroutineOven = Subroutine exampleProcedureOven NoOp

exampleSequence :: Step
exampleSequence =
  Nested
    0
    ( fromList
        [ undefined
        ]
    )

singleStep :: Step
singleStep = Known 0 (Quanticle (Number 42))

checkEvaluationPhase :: Spec
checkEvaluationPhase = do
  describe "Evaluator behaviour fundamentals" $ do
    it "single Step of Known Quantity evaluates" $ do
      result <- testEvaluation (evaluateStep singleStep)
      result `shouldBe` Quanticle (Number 42)
