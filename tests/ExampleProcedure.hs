{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ExampleProcedure where

import Core.Text.Rope ()
import Core.Text.Utilities ()
import Core.Program.Execute hiding (None)
import Core.Program.Logging

import Technique.Builtins
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
        , procedureLabel = Just (Markdown "Set oven temperature")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Execute (Text "builtinProcedure!") ]
        }

exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    i = Type "Ingredients"
    o = Type "Turkey"
    celsius = "°C"
    chef = Role (Identifier "chef")
    block = Block
                [ Execute (Restriction chef (Block
                    [ Assignment
                        [Identifier "preheat"]
                        (Application
                            (Identifier "oven")
                            (Grouping (Amount (Quantity (Decimal 180 0) (Decimal 0 0) 0 celsius))))
                    , Execute
                        (Application
                            (Identifier "task")
                            (Text "Bacon strips onto bird"))
                    , Execute
                        (Variable [Identifier "preheat"])
                    , Execute
                        (Undefined)
                    , Blank
                    , Execute
                        (Operation WaitBoth
                            (Variable [Identifier "w1"])
                            (Grouping (Operation WaitEither
                                (Variable [Identifier "w2"])
                                (Variable [Identifier "w3"]))))
                    , Blank
                    , Execute (Variable [Identifier "v1"])
                    , Series
                    , Execute (Variable [Identifier "v2"])
                    , Assignment
                        [Identifier "temp"]
                        (Application
                            (Identifier "record")
                            (Text "Probe bird temperature"))
                    , Execute
                        (Object
                            (Tablet
                                [ Binding "Final temperature" (Variable [Identifier "temp"]) ]))
                    ])
                )]
  in
    Procedure
        { procedureName = Identifier "roast_turkey"
        , procedureParams = [Identifier "i", Identifier "j", Identifier "k"]
        , procedureInput = [i]
        , procedureOutput = [o]
        , procedureLabel = Just (Markdown "Roast Turkey")
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
