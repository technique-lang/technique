{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec)

import Core.System

import CheckConcreteSyntax hiding (main)
import CheckSkeletonParser hiding (main)
import CheckQuantityBehaviour hiding (main)
import CheckTranslationPhase hiding (main)
import CheckEvaluationPhase hiding (main)

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkQuantityBehaviour
    checkConcreteSyntax
    checkSkeletonParser
    checkTranslationPhase
    checkEvaluationPhase
