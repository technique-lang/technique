{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckAbstractSyntax
    ( checkAbstractSyntax
    , main
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.Text.Utilities ()
-- import Data.Text.Prettyprint.Doc (Pretty(pretty))
import Core.Program.Execute hiding (None)
import Core.Program.Logging
import Data.Maybe (fromJust)
import Test.Hspec

import Technique.Language
import Technique.Quantity
import Technique.Formatter ()

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
        , procedureInput = Type "Temperature"
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
        , procedureInput = Type "Text"
        , procedureOutput = Type "()" -- ?
        , procedureLabel = Just (Markdown "A task")
        , procedureDescription = Nothing
        , procedureBlock = Block [ Execute (Literal (Text "builtinProcedure!")) ]
        }

builtinProcedureRecord :: Procedure
builtinProcedureRecord =
    Procedure
        { procedureName = "record"
        , procedureInput = Type "Text"
        , procedureOutput = Type "Text" -- ?
        , procedureLabel = Just (Markdown "Record")
        , procedureDescription = Just (Markdown "Record a quantity")
        , procedureBlock = Block [ Execute (Literal (Text "builtinProcedure!")) ]
        }


exampleRoastTurkey :: Procedure
exampleRoastTurkey =
  let
    i = Type { typeName = "Ingredients" }
    o = Type { typeName = "Turkey" }
    celsius = fromJust (lookupKeyValue "°C" units)
    block = Block
                [ Assignment
                    (Name "preheat")
                    (Application
                        exampleProcedureOven
                        (Literal (Quantity 180 celsius)))
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
                    (Operation (Operator "&") (Variable (Name "w1")) (Variable (Name "w2")))
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
                ]
  in
    Procedure
        { procedureName = "roast_turkey"
        , procedureInput = i
        , procedureOutput = o
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
            procedureName builtinProcedureTask `shouldBe` "task"

        it "Procedure's function name is correct" $ do
            procedureName exampleRoastTurkey `shouldBe` "roast_turkey"

main :: IO ()
main = execute $ do
    setVerbosityLevel Event
    writeR exampleRoastTurkey
--  writeS (pretty exampleRoastTurkey)
