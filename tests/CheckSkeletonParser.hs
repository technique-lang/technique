{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckSkeletonParser
    ( checkSkeletonParser
    )
where

import Core.Text
import Test.Hspec
import Text.Megaparsec

import Technique.Parser

checkSkeletonParser :: Spec
checkSkeletonParser = do
    describe "Parse bookfile format" $ do
        it "Correctly parses a complete first line" $ do
            parseMaybe parseMagicLine "% technique v2\n" `shouldBe` Just 2
        it "Errors if first line has incorrect syntax" $ do
            parseMaybe parseMagicLine "%\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "%technique\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique \n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique v\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique  v2\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique v2 asdf\n" `shouldBe` Nothing

        it "Correctly parses a complete technique program" $ do
            parseMaybe parseBookfile [quote|
% technique v0
            |] `shouldBe` Just ()
