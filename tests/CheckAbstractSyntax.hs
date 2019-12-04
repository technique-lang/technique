{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckAbstractSyntax
    ( checkAbstractSyntax
    )
where

import Core.Text.Rope ()
import Core.Text.Utilities
import Data.Text.Prettyprint.Doc (line)
import Test.Hspec

import Technique.Builtins
import Technique.Formatter
import Technique.Internal
import Technique.Language
import Technique.Quantity


import ExampleProcedure hiding (main)

{-|
When we set the expectation using a [quote| ... |] here doc we get a
trailing newline that is not present in the rendered AST element. So
administratively add one here.
-}
renderTest :: Render a => a -> String
renderTest x = show (intoDocA x <> line)

{-|
These are less tests than a body of code that exercises construction of
an abstract syntax tree.
-}
checkAbstractSyntax :: Spec
checkAbstractSyntax = do
    describe "Constructions matching intended language design" $ do
        it "key builtin procedures are available" $ do
            procedureName (primitiveSource builtinProcedureTask) `shouldBe` Identifier "task"

        it "procedure's function name is correct" $ do
            procedureName exampleRoastTurkey `shouldBe` Identifier "roast_turkey"

    describe "Rendering of abstract syntax tree to Technique language" $ do
        it "renders a list as tuple" $ do
            show (commaCat [Identifier "one", Identifier "two", Identifier "three"])
                `shouldBe` "one,two,three"
            show (commaCat ([] :: [Identifier]))
                `shouldBe` ""

        it "renders a tablet as expected" $
          let
            tablet = Tablet
                        [ Binding "Final temperature" (Variable [Identifier "temp"])
                        , Binding "Cooking time" (Grouping (Amount (Quantity (Decimal 3 0) (Decimal 0 0) 0 "hr")))
                        ]
          in do
            renderTest tablet `shouldBe` [quote|
[
    "Final temperature" ~ temp
    "Cooking time" ~ (3 hr)
]
|]

    describe "Rendering of a Block" $ do
        it "renders a normal block with indentation" $
          let
            b = Block
                [ Execute (Variable [Identifier "x"])
                ]
          in do
            renderTest b `shouldBe` [quote|
{
    x
}
|]

    describe "Rendering of a Procedure" $ do
        it "renders a function signature correctly" $
          let
            p = emptyProcedure
                    { procedureName = Identifier "f"
                    , procedureInput = [Type "X"]
                    , procedureOutput = [Type "Y"]
                    , procedureBlock = Block [Execute (Variable [Identifier "z"])]
                    }
          in do
            renderTest p `shouldBe` [quote|
    f : X -> Y
    {
        z
    }
|]
