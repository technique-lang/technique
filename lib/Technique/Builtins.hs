{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
This is the beginnings of the standard library.
-}
-- Do these need descriptions? Not really, unless this becomes a form of
-- online help. Such text would not be used in overviews, so we'll see if
-- we ever need them.
module Technique.Builtins where

import Core.Data.Structures
import Core.Text.Rope ()

import Technique.Internal
import Technique.Language

builtinProcedures :: Map Identifier Function
builtinProcedures = intoMap (fmap (\p -> (functionName p, p))
    [ builtinProcedureWaitEither
    , builtinProcedureWaitBoth
    , builtinProcedureCombineValues
    , builtinProcedureTask
    , builtinProcedureRecord
    ])

builtinProcedureTask :: Function
builtinProcedureTask = Primitive
    Procedure
        { procedureName = Identifier "task"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = [Type "()"] -- ?
        , procedureTitle = Just (Markdown "Task")
        , procedureDescription = Just (Markdown "A task to be executed by the person carrying out this role.")
        , procedureBlock = Block []
        }
    undefined

builtinProcedureRecord :: Function
builtinProcedureRecord = Primitive
    Procedure
        { procedureName = Identifier "record"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = [Type "Text"] -- ?
        , procedureTitle = Just (Markdown "Record")
        , procedureDescription = Just (Markdown "Input from the user to be parsed as a quantity.")
        , procedureBlock = Block []
        }
    undefined

-- the '|' operation
builtinProcedureWaitEither :: Function
builtinProcedureWaitEither = Primitive
    Procedure
        { procedureName = Identifier "wait_either"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "()"]
        , procedureTitle = Just (Markdown "Wait Either")
        , procedureDescription = Just (Markdown "Wait for either of two values to be ready.")
        , procedureBlock = Block []
        }
    undefined

-- the '&' operation
builtinProcedureWaitBoth :: Function
builtinProcedureWaitBoth = Primitive
    Procedure
        { procedureName = Identifier "wait_both"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "()"]
        , procedureTitle = Just (Markdown "Wait Both")
        , procedureDescription = Just (Markdown "Wait for two values to both be ready.")
        , procedureBlock = Block []
        }
    undefined

-- the '+' operation
builtinProcedureCombineValues :: Function
builtinProcedureCombineValues = Primitive
    Procedure
        { procedureName = Identifier "combine_values"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "*"]
        , procedureTitle = Just (Markdown "Combine Two Values")
        , procedureDescription = Just (Markdown "Combine two values. This will involve coersion if the concrete types differ.")
        , procedureBlock = Block []
        }
    undefined
