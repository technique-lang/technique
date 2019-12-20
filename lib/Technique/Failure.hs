{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Error messages from compiling.
-}
-- I generally try to avoid modules full of (only) types but these are here
-- so the can be shared in both Technique.Translate and Technique.Builtins.
module Technique.Failure where

import Core.System.Base
import Core.System.Pretty
import Core.Text.Rope
import Core.Text.Utilities

import Technique.Language
import Technique.Formatter

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
    displayException = fromRope . render 78 

-- TODO upgrade this to (Doc ann) so we can get prettier error messages.

instance Render CompilerFailure where
    type Token CompilerFailure = TechniqueToken
    colourize = colourizeTechnique
    intoDocA failure = case failure of
        ParsingFailed err -> pretty err
        VariableAlreadyInUse _ i -> "Variable by the name of '" <> intoDocA i <> "' already defined."
        ProcedureAlreadyDeclared _ i -> "Procedure by the name of '" <> intoDocA i <> "' already declared."
        CallToUnknownProcedure _ i -> "Call to unknown procedure '" <> intoDocA i <> "'."
        UseOfUnknownIdentifier (offset,statement) i ->
            pretty offset <> ": " <> intoDocA statement <> line <> "Variable '" <> intoDocA i <> "' not in scope."
        EncounteredUndefined _ -> "Encountered 'undefined' marker."
