{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ExampleProcedure where

import Core.Text.Rope ()
import Core.Text.Utilities ()
import Core.Program.Execute hiding (None)
import Core.Program.Logging

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
        { procedureName = Identifier "oven"
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
        { procedureName = Identifier "task"
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
        { procedureName = Identifier "record"
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
    celsius = "°C"
    chef = Role "chef"
    block = Block
                [ Attribute chef (Block
                    [ Assignment
                        (Identifier "preheat")
                        (Application
                            (Identifier "oven")
                            (Grouping (Literal (Quantity 180 celsius))))
                    , Execute
                        (Application
                            (Identifier "task")
                            (Literal (Text "Bacon strips onto bird")))
                    , Execute
                        (Variable (Identifier "preheat"))
                    , Execute
                        (Literal Undefined)
                    , Blank
                    , Execute
                        (Operation WaitBoth
                            (Variable (Identifier "w1"))
                            (Grouping (Operation WaitEither
                                (Variable (Identifier "w2"))
                                (Variable (Identifier "w3")))))
                    , Blank
                    , Execute (Variable (Identifier "v1"))
                    , Series
                    , Execute (Variable (Identifier "v2"))
                    , Assignment
                        (Identifier "temp")
                        (Application
                            (Identifier "record")
                            (Literal (Text "Probe bird temperature")))
                    , Execute
                        (Object
                            (Tablet
                                [ Binding "Final temperature" (Variable (Identifier "temp")) ]))
                    ])
                ]
  in
    Procedure
        { procedureName = Identifier "roast_turkey"
        , procedureParams = [Identifier "i", Identifier "j", Identifier "k"]
        , procedureInput = [i]
        , procedureOutput = o
        , procedureLabel = Just (Markdown "Roast Turkey")
        , procedureDescription = Nothing
        , procedureBlock = block
        }

main :: IO ()
main = execute $ do
    writeR exampleRoastTurkey
