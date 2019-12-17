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
        , procedureBlock = Block [ (0, Execute (Application (Identifier "task") (Text "Set oven temperature!")) )]
        }

exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    i = Type "Ingredients"
    o = Type "Turkey"
    celsius = "°C"
    chef = Role (Identifier "chef")
    block = Block
                [ (0, Execute (Restriction chef (Block
                    [ (0, Assignment
                        [Identifier "preheat"]
                        (Application
                            (Identifier "oven")
                            (Grouping (Amount (Quantity (Decimal 180 0) (Decimal 0 0) 0 celsius)))))
                    , (0, Execute
                        (Application
                            (Identifier "task")
                            (Text "Bacon strips onto bird")))
                    , (0, Execute
                        (Variable [Identifier "preheat"]))
                    , (0, Execute
                        (Undefined))
                    , (0, Blank)
                    , (0, Execute
                        (Operation WaitBoth
                            (Variable [Identifier "w1"])
                            (Grouping (Operation WaitEither
                                (Variable [Identifier "w2"])
                                (Variable [Identifier "w3"])))))
                    , (0, Blank)
                    , (0, Execute (Variable [Identifier "v1"]))
                    , (0, Series)
                    , (0, Execute (Variable [Identifier "v2"]))
                    , (0, Assignment
                        [Identifier "temp"]
                        (Application
                            (Identifier "record")
                            (Text "Probe bird temperature")))
                    , (0, Execute
                        (Object
                            (Tablet
                                [ Binding (Label "Final temperature") (Variable [Identifier "temp"]) ])))
                    ]))
                )]
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
