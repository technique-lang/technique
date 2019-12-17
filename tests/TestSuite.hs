{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec)

import Core.System

import CheckAbstractSyntax
import CheckSkeletonParser hiding (main)
import CheckQuantityBehaviour hiding (main)
import CheckTranslationStage hiding (main)

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkQuantityBehaviour
    checkAbstractSyntax
    checkSkeletonParser
    checkTranslationStage
