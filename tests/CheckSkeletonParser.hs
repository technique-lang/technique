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

import Technique.Language
import Technique.Parser

checkSkeletonParser :: Spec
checkSkeletonParser = do
    describe "Parse procfile header" $ do
        it "correctly parses a complete magic line" $ do
            parseMaybe pMagicLine "% technique v2\n" `shouldBe` Just 2
        it "errors if magic line has incorrect syntax" $ do
            parseMaybe pMagicLine "%\n" `shouldBe` Nothing
            parseMaybe pMagicLine "%technique\n" `shouldBe` Nothing
            parseMaybe pMagicLine "% technique\n" `shouldBe` Nothing
            parseMaybe pMagicLine "% technique \n" `shouldBe` Nothing
            parseMaybe pMagicLine "% technique v\n" `shouldBe` Nothing
            parseMaybe pMagicLine "% technique  v2\n" `shouldBe` Nothing
            parseMaybe pMagicLine "% technique v2 asdf\n" `shouldBe` Nothing

        it "correctly parses an SPDX header line" $ do
            parseMaybe pSpdxLine "! BSD-3-Clause\n" `shouldBe` Just ("BSD-3-Clause",Nothing)
        it "correctly parses an SPDX header line with Copyright" $ do
            parseMaybe pSpdxLine "! BSD-3-Clause, (c) 2019 Kermit le Frog\n" `shouldBe` Just ("BSD-3-Clause",Just "2019 Kermit le Frog")
        it "errors if SPDX line has incorrect syntax" $ do
            parseMaybe pSpdxLine "!\n" `shouldBe` Nothing
            parseMaybe pSpdxLine "!,\n" `shouldBe` Nothing
            parseMaybe pSpdxLine "! Public-Domain,\n" `shouldBe` Nothing
            parseMaybe pSpdxLine "! Public-Domain, (\n" `shouldBe` Nothing
            parseMaybe pSpdxLine "! Public-Domain, (c)\n" `shouldBe` Nothing
            parseMaybe pSpdxLine "! Public-Domain, (c) \n" `shouldBe` Nothing

        it "correctly parses a complete technique program header" $ do
            parseMaybe pProcfileHeader [quote|
% technique v0
! BSD-3-Clause
            |] `shouldBe` Just (Technique
                            { techniqueVersion = 0
                            , techniqueLicense = "BSD-3-Clause"
                            , techniqueCopyright = Nothing
                            , techniqueBody = []
                            })

    describe "Parses a proecdure function" $ do
        it "name parser handles valid identifiers" $ do
            parseMaybe pIdentifier "" `shouldBe` Nothing
            parseMaybe pIdentifier "i" `shouldBe` Just (Identifier "i")
            parseMaybe pIdentifier "ingredients" `shouldBe` Just (Identifier "ingredients")
            parseMaybe pIdentifier "roast_turkey" `shouldBe` Just (Identifier "roast_turkey")
            parseMaybe pIdentifier "1x" `shouldBe` Nothing
            parseMaybe pIdentifier "x1" `shouldBe` Just (Identifier "x1")

        it "type parser handles valid type names" $ do
            parseMaybe pType "" `shouldBe` Nothing
            parseMaybe pType "I" `shouldBe` Just (Type "I")
            parseMaybe pType "Ingredients" `shouldBe` Just (Type "Ingredients")
            parseMaybe pType "RoastTurkey" `shouldBe` Just (Type "RoastTurkey")
            parseMaybe pType "Roast_Turkey" `shouldBe` Nothing
            parseMaybe pType "2Dinner" `shouldBe` Nothing
            parseMaybe pType "Dinner3" `shouldBe` Just (Type "Dinner3")

        it "handles a name, parameters, and a type signature" $ do
            parseMaybe pProcedureDeclaration "roast_turkey i : Ingredients -> Turkey"
                `shouldBe` Just (Identifier "roast_turkey", [Identifier "i"], [Type "Ingredients"], Type "Turkey")
            parseMaybe pProcedureDeclaration "roast_turkey  i  :  Ingredients  ->  Turkey"
                `shouldBe` Just (Identifier "roast_turkey", [Identifier "i"], [Type "Ingredients"], Type "Turkey")
            parseMaybe pProcedureDeclaration "roast_turkey:Ingredients->Turkey"
                `shouldBe` Just (Identifier "roast_turkey", [], [Type "Ingredients"], Type "Turkey")
