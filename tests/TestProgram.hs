{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec)

import Core.System

import CheckSkeletonParser
import CheckAbstractSyntax hiding (main)

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkAbstractSyntax
    checkSkeletonParser
