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

builtins :: Map Identifier Primitive
builtins = intoMap (fmap (\p -> (procedureName (primitiveSource p), p))
    [ builtinProcedureWaitEither
    , builtinProcedureWaitBoth
    , builtinProcedureCombineValues
    , builtinProcedureTask
    , builtinProcedureRecord
    ])

builtinProcedureTask :: Primitive
builtinProcedureTask = Primitive
    { primitiveSource = Procedure
        { procedureName = Identifier "task"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = [Type "()"] -- ?
        , procedureLabel = Just (Markdown "Task")
        , procedureDescription = Just (Markdown "A task to be executed by the person carrying out this role.")
        , procedureBlock = Block []
        }
    , primitiveAction = undefined
    }

builtinProcedureRecord :: Primitive
builtinProcedureRecord = Primitive
    { primitiveSource = Procedure
        { procedureName = Identifier "record"
        , procedureParams = []
        , procedureInput = [Type "Text"]
        , procedureOutput = [Type "Text"] -- ?
        , procedureLabel = Just (Markdown "Record")
        , procedureDescription = Just (Markdown "Input from the user to be parsed as a quantity.")
        , procedureBlock = Block []
        }
    , primitiveAction = undefined
    }

-- the '|' operation
builtinProcedureWaitEither :: Primitive
builtinProcedureWaitEither = Primitive
    { primitiveSource = Procedure
        { procedureName = Identifier "wait_either"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "()"]
        , procedureLabel = Just (Markdown "Wait Either")
        , procedureDescription = Just (Markdown "Wait for either of two values to be ready.")
        , procedureBlock = Block []
        }
    , primitiveAction = undefined
    }

-- the '&' operation
builtinProcedureWaitBoth :: Primitive
builtinProcedureWaitBoth = Primitive
    { primitiveSource = Procedure
        { procedureName = Identifier "wait_both"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "()"]
        , procedureLabel = Just (Markdown "Wait Both")
        , procedureDescription = Just (Markdown "Wait for two values to both be ready.")
        , procedureBlock = Block []
        }
    , primitiveAction = undefined
    }

-- the '+' operation
builtinProcedureCombineValues :: Primitive
builtinProcedureCombineValues = Primitive
    { primitiveSource = Procedure
        { procedureName = Identifier "combine_values"
        , procedureParams = []
        , procedureInput = [Type "*", Type "*"]
        , procedureOutput = [Type "*"]
        , procedureLabel = Just (Markdown "Combine Two Values")
        , procedureDescription = Just (Markdown "Combine two values. This will involve coersion if the concrete types differ.")
        , procedureBlock = Block []
        }
    , primitiveAction = undefined
    }
