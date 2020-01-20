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
        , procedureOutput = [Type "()"] -- ?
        , procedureTitle = Nothing
        , procedureDescription = Nothing
        , procedureBlock = Block [ Execute 0 (Application 0 (Identifier "task") (Text 0 "Set oven temperature!")) ]
        }

exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    i = Type "Ingredients"
    o = Type "Turkey"
    celsius = "°C"
    chef = Role (Identifier "chef")
    block = Block
                [ Execute 0 (Restriction 0 chef (Block
                    [ Assignment 0
                        [Identifier "preheat"]
                        (Application 0
                            (Identifier "oven")
                            (Grouping 0 (Amount 0 (Quantity (Decimal 180 0) (Decimal 0 0) 0 celsius))))
                    , Execute 0
                        (Application 0
                            (Identifier "task")
                            (Text 0 "Bacon strips onto bird"))
                    , Execute 0
                        (Variable 0 [Identifier "preheat"])
                    , Execute 0
                        (Undefined 0)
                    , Blank 0
                    , Execute 0
                        (Operation 0 WaitBoth
                            (Variable 0 [Identifier "w1"])
                            (Grouping 0 (Operation 0 WaitEither
                                (Variable 0 [Identifier "w2"])
                                (Variable 0 [Identifier "w3"]))))
                    , Blank 0
                    , Execute 0 (Variable 0 [Identifier "v1"])
                    , Series 0
                    , Execute 0 (Variable 0 [Identifier "v2"])
                    , Assignment 0
                        [Identifier "temp"]
                        (Application 0
                            (Identifier "record")
                            (Text 0 "Probe bird temperature"))
                    , Execute 0
                        (Object 0
                            (Tablet
                                [ Binding (Label "Final temperature") (Variable 0 [Identifier "temp"]) ]))
                    ]))
                ]
  in
    Procedure
        { procedureName = Identifier "roast_turkey"
        , procedureParams = [Identifier "i", Identifier "j", Identifier "k"]
        , procedureInput = [i]
        , procedureOutput = [o]
        , procedureTitle = Just (Markdown "Roast Turkey")
        , procedureDescription = Nothing
        , procedureBlock = block
        }

exampleTechnique :: Technique
exampleTechnique =
    Technique
        { techniqueVersion = 0
        , techniqueLicense = "BSD-3-Clause"
        , techniqueCopyright = Just "2018 Allicin Wonderland"
        , techniqueBody = [exampleRoastTurkey]
        }

main :: IO ()
main = execute $ do
    writeR exampleTechnique
