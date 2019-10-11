{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckAbstractSyntax
    ( checkAbstractSyntax
    , main
    )
where

import Core.Data.Structures
import Core.Text.Rope
import Core.Text.Utilities
import Data.Text.Prettyprint.Doc
    ( Doc, Pretty(pretty)
    )
import Core.Program.Execute
import Core.Program.Logging
import Data.Maybe (fromJust)
import Test.Hspec

import Technique.Language
import Technique.Quantity
import Technique.Formatter

{-
    roast_turkey i : Ingredients -> Turkey
    {
        @chef
        {
            preheat = oven (180 °C)
            task "Bacon strips onto bird"
            wait preheat

            task "Put bird into oven"

            t = timer (3 h)
            wait t

            temp = record "Probe bird temperature"
            [
                "Final temperature" ~ temp
            ]
        }
-}

exampleProcedureOven :: Procedure
exampleProcedureOven =
    Procedure
        { procedureName = "oven"
        , procedureInput = Type "Temperature"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "Set oven temperature")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Result (Literal (Text "builtinProcedure!")) ]
        }


-- TODO these two are actual builin standard library procedures, so a
-- future change to this test case will involve doing a lookup of these
-- names in some environment or context.

builtinProcedureTask :: Procedure
builtinProcedureTask =
    Procedure
        { procedureName = "task"
        , procedureInput = Type "Text"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "A task")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Result (Literal (Text "builtinProcedure!")) ]
        }

builtinProcedureWait :: Procedure
builtinProcedureWait =
    Procedure
        { procedureName = "wait"
        , procedureInput = Type "*"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "Wait")
        , procedureDescription = Just (Markdown "Wait for a procedure to complete.")
        , procedureBlock = Block [ Result (Literal (Text "builtinProcedure!")) ]
        }


exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    input = Type { typeName = "Ingredients" }
    output = Type { typeName = "Turkey" }
    celsius = fromJust (lookupKeyValue "°C" units)
    block = Block
                [ Assignment (Variable "preheat") (Application exampleProcedureOven (Literal (Quantity 180 celsius)))
                , Execute (Application builtinProcedureTask (Literal (Text "Bacon strips onto bird")))
                , Execute (Application builtinProcedureWait (Evaluate (Variable "preheat")))
                , Result (Literal Nil)
                ]
  in
    Procedure
        { procedureName = "roast_turkey"
        , procedureInput = input
        , procedureOutput = output
        , procedureLabel = Just (Markdown "Roast Turkey")
        , procedureDescription = Nothing
        , procedureBlock = block
        }


{-|
These are less tests than a body of code that exercises construction of
an abstract syntax tree.
-}
checkAbstractSyntax :: Spec
checkAbstractSyntax = do
    describe "Constructions matching intended language design" $ do
        it "Key builtinProcedure procedures are available" $ do
            procedureName builtinProcedureWait `shouldBe` "wait"

        it "Procedure's function name is correct" $ do
            procedureName exampleRoastTurkey `shouldBe` "roast_turkey"

main :: IO ()
main = execute $ do
    writeR exampleRoastTurkey
    writeS (pretty exampleRoastTurkey)
