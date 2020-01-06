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
import Data.List.NonEmpty
import qualified Data.Set as OrdSet
import qualified Data.Text as T
import Text.Megaparsec (PosState(..), SourcePos(..))
import Text.Megaparsec.Error
    ( ParseError(..)
    , ErrorFancy(..)
    , ParseErrorBundle(..)
    , errorBundlePretty
    , ShowErrorComponent(..)
    )
import Text.Megaparsec.Pos (Pos, mkPos)

import Technique.Language
import Technique.Formatter

data Source = Source
    { sourceContents :: T.Text                   -- whatever parser is expecting
    , sourceFilename :: FilePath
    , sourceStatement :: Statement
    , sourceOffset :: Offset
    }
    deriving (Eq,Ord,Show)

emptySource :: Source
emptySource = Source
    { sourceContents = T.empty
    , sourceFilename = "<undefined>"
    , sourceStatement = Blank
    , sourceOffset = -1
    }

data FailureReason
    = ParsingFailed String              -- FIXME change to ParseErrorSomethingErOther
    | VariableAlreadyInUse Identifier
    | ProcedureAlreadyDeclared Identifier
    | CallToUnknownProcedure Identifier
    | UseOfUnknownIdentifier Identifier
    | EncounteredUndefined
    deriving (Eq,Ord,Show)

instance Enum FailureReason where
    fromEnum x = case x of
        ParsingFailed _ -> 1
        VariableAlreadyInUse _ -> 2
        ProcedureAlreadyDeclared _ -> 3
        CallToUnknownProcedure _ -> 4
        UseOfUnknownIdentifier _ -> 5
        EncounteredUndefined -> 6
    toEnum = undefined

instance Exception CompilationError where
    displayException = fromRope . render 78 

data CompilationError = CompilationError Source FailureReason
    deriving Show

exitCodeFor :: CompilationError -> Int
exitCodeFor (CompilationError _ reason) = fromEnum reason

-- TODO upgrade this to (Doc ann) so we can get prettier error messages.

instance Render FailureReason where
    type Token FailureReason = TechniqueToken
    colourize = colourizeTechnique
    intoDocA failure = case failure of
        ParsingFailed err -> pretty err
        VariableAlreadyInUse i -> "Variable by the name of '" <> intoDocA i <> "' already defined."
        ProcedureAlreadyDeclared i -> "Procedure by the name of '" <> intoDocA i <> "' already declared."
        CallToUnknownProcedure i -> "Call to unknown procedure '" <> intoDocA i <> "'."
        UseOfUnknownIdentifier i -> "Variable '" <> intoDocA i <> "' not in scope."
        EncounteredUndefined -> "Encountered 'undefined' marker."

{-
            let
                statement = sourceStatement source
                offset = sourceOffset source
            in
                pretty offset <> ": " <> intoDocA statement <> line <>
-}

instance Render CompilationError where
    type Token CompilationError = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (CompilationError source reason) = intoDocA reason


instance ShowErrorComponent FailureReason where
    showErrorComponent = fromRope . render 78

{-|
In order to have consistently formatted "compiler" failure messages, we
jump through the hoops to use megaparsec's parse error message machinery.
-}
makeErrorBundle :: (FilePath,T.Text,Offset,FailureReason) -> ParseErrorBundle T.Text FailureReason
makeErrorBundle (filename,input,offset,failure) =
  let
    fancy = ErrorCustom failure
    errors = FancyError offset (OrdSet.singleton fancy) :| []
    bundle = ParseErrorBundle
        { bundleErrors = errors
        , bundlePosState = PosState
            { pstateInput = input
            , pstateOffset = 0
            , pstateSourcePos = SourcePos
                { sourceName = filename
                , sourceLine = mkPos 1
                , sourceColumn = mkPos 1
                }
            , pstateTabWidth = mkPos 4
            , pstateLinePrefix = ""
            }
        }
  in
    bundle
