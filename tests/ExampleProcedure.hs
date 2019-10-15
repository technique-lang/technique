{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ExampleProcedure where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.Text.Utilities ()
import Core.Program.Execute hiding (None)
import Core.Program.Logging
import Data.Maybe (fromJust)

import Technique.Language
import Technique.Quantity
import Technique.Formatter () -- Render instances for main function

{-
    roast_turkey i : Ingredients -> Turkey
    {
        @chef
        {
            preheat = oven (180 °C)
            task "Bacon strips onto bird"
            preheat

            task "Put bird into oven"

            t = timer (3 h)
            t

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
        , procedureParams = []
        , procedureInput = [Type "Temperature"]
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "Set oven temperature")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Execute (Literal (Text "builtinProcedure!")) ]
        }


-- TODO these two are actual builin standard library procedures, so a
-- future change to this test case will involve doing a lookup of these
-- names in some environment or context.

builtinProcedureTask :: Procedure
builtinProcedureTask =
    Procedure
        { procedureName = "task"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "A task")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Execute (Literal (Text "builtinProcedure!")) ]
        }

builtinProcedureRecord :: Procedure
builtinProcedureRecord =
    Procedure
        { procedureName = "record"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = Type "Text" -- ?
        , procedureLabel = Just (Markdown "Record")
        , procedureDescription = Just (Markdown "Record a quantity")
        , procedureBlock = Block [ Execute (Literal (Text "builtinProcedure!")) ]
        }


exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    i = Type "Ingredients"
    o = Type "Turkey"
    celsius = fromJust (lookupKeyValue "°C" units)
    chef = Role "chef"
    block = Block
                [ Attribute chef (Block
                    [ Assignment
                        (Name "preheat")
                        (Application
                            exampleProcedureOven
                            (Grouping (Literal (Quantity 180 celsius))))
                    , Execute
                        (Application
                            builtinProcedureTask
                            (Literal (Text "Bacon strips onto bird")))
                    , Execute
                        (Variable (Name "preheat"))
                    , Execute
                        (Literal None)
                    , Blank
                    , Execute
                        (Operation (Operator "&")
                            (Variable (Name "w1"))
                            (Grouping (Operation (Operator "|")
                                (Variable (Name "w2"))
                                (Variable (Name "w3")))))
                    , Blank
                    , Assignment
                        (Name "temp")
                        (Application
                            builtinProcedureRecord
                            (Literal (Text "Probe bird temperature")))
                    , Execute
                        (Table
                            (Tablet
                                [ Binding "Final temperature" (Variable (Name "temp")) ]))
                    ])
                ]
  in
    Procedure
        { procedureName = "roast_turkey"
        , procedureParams = [Name "i", Name "j", Name "k"]
        , procedureInput = [i]
        , procedureOutput = o
        , procedureLabel = Just (Markdown "Roast Turkey")
        , procedureDescription = Nothing
        , procedureBlock = block
        }

main :: IO ()
main = execute $ do
    writeR exampleRoastTurkey
