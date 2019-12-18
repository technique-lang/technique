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
import Core.Text.Utilities (render)

import Technique.Language
import Technique.Formatter ()

data CompilerFailure
    = ParsingFailed String              -- FIXME change to ParseErrorSomethingErOther
    | VariableAlreadyInUse (Offset,Statement) Identifier
    | ProcedureAlreadyDeclared (Offset,Statement) Identifier
    | CallToUnknownProcedure (Offset,Statement) Identifier
    | UseOfUnknownIdentifier (Offset,Statement) Identifier
    | EncounteredUndefined (Offset,Statement)
    deriving (Eq,Show)

instance Enum CompilerFailure where
    fromEnum x = case x of
        ParsingFailed _ -> 1
        VariableAlreadyInUse _ _ -> 2
        ProcedureAlreadyDeclared _ _ -> 3
        CallToUnknownProcedure _ _ -> 4
        UseOfUnknownIdentifier _ _ -> 5
        EncounteredUndefined _ -> 6
    toEnum = undefined

instance Exception CompilerFailure where
    displayException = fromRope . renderFailure

-- TODO upgrade this to (Doc ann) so we can get prettier error messages.

renderFailure :: CompilerFailure -> Rope
renderFailure e = case e of
    ParsingFailed err -> intoRope err
    VariableAlreadyInUse _ i -> "Variable by the name of '" <> unIdentifier i <> "' already defined."
    ProcedureAlreadyDeclared _ i -> "Procedure by the name of '" <> unIdentifier i <> "' already declared."
    CallToUnknownProcedure _ i -> "Call to unknown procedure '" <> unIdentifier i <> "'."
    UseOfUnknownIdentifier (offset,statement) i ->
        intoRope (show offset) <> ": " <> render 78 statement <> "\nVariable '" <> unIdentifier i <> "' not in scope."
    EncounteredUndefined _ -> "Encountered 'undefined' marker."
