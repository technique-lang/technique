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
    describe "Parse procfile header" $ do
        it "Correctly parses a complete magic line" $ do
            parseMaybe parseMagicLine "% technique v2\n" `shouldBe` Just 2
        it "Errors if magic line has incorrect syntax" $ do
            parseMaybe parseMagicLine "%\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "%technique\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique \n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique v\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique  v2\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% technique v2 asdf\n" `shouldBe` Nothing

        it "Correctly parses an SPDX header line" $ do
            parseMaybe parseSpdxLine "! BSD-3-Clause\n" `shouldBe` Just ("BSD-3-Clause",Nothing)
        it "Correctly parses an SPDX header line with Copyright" $ do
            parseMaybe parseSpdxLine "! BSD-3-Clause, (c) 2019 Kermit le Frog\n" `shouldBe` Just ("BSD-3-Clause",Just "2019 Kermit le Frog")
        it "Errors if SPDX line has incorrect syntax" $ do
            parseMaybe parseSpdxLine "!\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "!,\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "! Public-Domain,\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "! Public-Domain, (\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "! Public-Domain, (c)\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "! Public-Domain, (c) \n" `shouldBe` Nothing

        it "Correctly parses a complete technique program header" $ do
            parseMaybe parseProcfileHeader [quote|
% technique v0
! BSD-3-Clause
            |] `shouldBe` Just ()
