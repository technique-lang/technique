{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec)

import CheckSkeletonParser
import Core.System

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkSkeletonParser
