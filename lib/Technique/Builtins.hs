{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
This is the beginnings of the standard library.
-}
-- Do these need descriptions? Not really, unless this becomes a form of
-- online help. Such text would not be used in overviews, so we'll see if
-- we ever need them.
module Technique.Builtins where

import Core.Text.Rope ()

import Technique.Language


-- TODO these need to be registered somewhere?

builtinProcedureTask :: Procedure
builtinProcedureTask =
    Procedure
        { procedureName = Identifier "task"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = [Type "()"] -- ?
        , procedureLabel = Just (Markdown "Task")
        , procedureDescription = Just (Markdown "A task to be executed by the person carrying out this role.")
        , procedureBlock = Block []
        }

builtinProcedureRecord :: Procedure
builtinProcedureRecord =
    Procedure
        { procedureName = Identifier "record"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = [Type "Text"] -- ?
        , procedureLabel = Just (Markdown "Record")
        , procedureDescription = Just (Markdown "Input from the user to be parsed as a quantity.")
        , procedureBlock = Block []
        }

-- the '|' operation
builtinProcedureWaitEither :: Procedure
builtinProcedureWaitEither =
    Procedure
        { procedureName = Identifier "wait_either"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "()"]
        , procedureLabel = Just (Markdown "Wait Either")
        , procedureDescription = Just (Markdown "Wait for either of two values to be ready.")
        , procedureBlock = Block []
        }

-- the '&' operation
builtinProcedureWaitBoth :: Procedure
builtinProcedureWaitBoth =
    Procedure
        { procedureName = Identifier "wait_both"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "()"]
        , procedureLabel = Just (Markdown "Wait Both")
        , procedureDescription = Just (Markdown "Wait for two values to both be ready.")
        , procedureBlock = Block []
        }

-- the '+' operation
builtinProcedureCombineValues :: Procedure
builtinProcedureCombineValues =
    Procedure
        { procedureName = Identifier "combine_values"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "*"]
        , procedureLabel = Just (Markdown "Combine Two Values")
        , procedureDescription = Just (Markdown "Combine two values. This will involve coersion if the concrete types differ.")
        , procedureBlock = Block []
        }
