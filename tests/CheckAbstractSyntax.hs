{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckAbstractSyntax
    ( checkAbstractSyntax
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Data.Maybe (fromJust)
import Test.Hspec

import Technique.Language
import Technique.Quantity

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

oven :: Procedure
oven =
    Procedure
        { procedureName = "oven"
        , procedureInput = Type "Temperature"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "Set oven temperature")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Result (Literal (Text "builtin!")) ]
        }

task :: Procedure
task =
    Procedure
        { procedureName = "task"
        , procedureInput = Type "Text"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "A task")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Result (Literal (Text "builtin!")) ]
        }

wait :: Procedure
wait =
    Procedure
        { procedureName = "wait"
        , procedureInput = Type "*"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "Wait")
        , procedureDescription = Just (Markdown "Wait for a procedure to complete.")
        , procedureBlock = Block [ Result (Literal (Text "builtin!")) ]
        }


exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    input = Type { typeName = "Ingredients" }
    output = Type { typeName = "Turkey" }
    celsius = fromJust (lookupKeyValue "°C" units)
    block = Block
                [ Assignment (Variable "preheat") (Application oven (Literal (Quantity 180 celsius)))
                , Execute (Application task (Literal (Text "Bacon strips onto bird")))
                , Execute (Application wait (Evaluate (Variable "preheat")))
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

checkAbstractSyntax :: Spec
checkAbstractSyntax = do
    describe "Constructions matching intended language design" $ do
        it "Correctly parses a complete magic line" $ do
            procedureName exampleRoastTurkey `shouldBe` "roast_turkey"