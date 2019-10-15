{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckAbstractSyntax
    ( checkAbstractSyntax
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.Text.Utilities
import Data.Maybe (fromJust)
import Data.Text.Prettyprint.Doc (line)
import Test.Hspec

import Technique.Language
import Technique.Quantity
import Technique.Formatter

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
            procedureName builtinProcedureTask `shouldBe` "task"

        it "procedure's function name is correct" $ do
            procedureName exampleRoastTurkey `shouldBe` "roast_turkey"

    describe "Rendering of abstract syntax tree to Technique language" $ do
        it "renders a list as tuple" $ do
            show (commaCat [Name "one", Name "two", Name "three"])
                `shouldBe` "one,two,three"

        it "renders a tablet as expected" $
          let
            hours = fromJust (lookupKeyValue "hr" units)
            tablet = Tablet
                        [ Binding "Final temperature" (Variable (Name "temp"))
                        , Binding "Cooking time" (Grouping (Literal (Quantity 3 hours)))
                        ]
          in do
            renderTest tablet `shouldBe` [quote|
[
    "Final temperature" ~ temp
    "Cooking time" ~ (3 hr)
]
|]

