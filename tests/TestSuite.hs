{-# LANGUAGE OverloadedStrings #-}

import CheckConcreteSyntax hiding (main)
import CheckQuantityBehaviour hiding (main)
import CheckSkeletonParser hiding (main)
import CheckTranslationPhase hiding (main)
import Core.System
import Test.Hspec (Spec, hspec)

main :: IO ()
main = do
  finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
  checkQuantityBehaviour
  checkConcreteSyntax
  checkSkeletonParser
  checkTranslationPhase
