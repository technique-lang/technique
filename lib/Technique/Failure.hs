{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Error messages from compiling.
-}
-- I generally try to avoid modules full of (only) types but these are here
-- so the can be shared in both Technique.Translate and Technique.Builtins.
module Technique.Failure where

import Core.System.Base
import Core.Text.Rope

import Technique.Language

data CompilerFailure
    = ParsingFailed String              -- FIXME change to ParseErrorSomethingErOther
    | VariableAlreadyInUse Identifier
    | ProcedureAlreadyDeclared Identifier
    | CallToUnknownProcedure Identifier
    | UseOfUnknownIdentifier Identifier
    | EncounteredUndefined
    deriving (Eq,Show)

instance Enum CompilerFailure where
    fromEnum x = case x of
        ParsingFailed _ -> 1
        VariableAlreadyInUse _ -> 2
        ProcedureAlreadyDeclared _ -> 3
        CallToUnknownProcedure _ -> 4
        UseOfUnknownIdentifier _ -> 5
        EncounteredUndefined -> 6
    toEnum = undefined

instance Exception CompilerFailure where
    displayException = fromRope . renderFailure

-- TODO upgrade this to (Doc ann) so we can get prettier error messages.

renderFailure :: CompilerFailure -> Rope
renderFailure e = case e of
    ParsingFailed err -> intoRope err
    VariableAlreadyInUse i -> "Variable by the name of '" <> unIdentifier i <> "' already defined."
    ProcedureAlreadyDeclared i -> "Procedure by the name of '" <> unIdentifier i <> "' already declared."
    CallToUnknownProcedure i -> "Call to unknown procedure '" <> unIdentifier i <> "'."
    UseOfUnknownIdentifier i -> "Variable '" <> unIdentifier i <> "' not in scope."
    EncounteredUndefined -> "Encountered 'undefined' marker."
