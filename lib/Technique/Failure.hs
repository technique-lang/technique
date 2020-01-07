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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as OrdSet
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (PosState(..), SourcePos(..), Stream)
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
    = InvalidSetup                         -- TODO placeholder
    | ParsingFailed                        -- FIXME change to ParseErrorSomethingErOther
    | VariableAlreadyInUse Identifier
    | ProcedureAlreadyDeclared Identifier
    | CallToUnknownProcedure Identifier
    | UseOfUnknownIdentifier Identifier
    | EncounteredUndefined
    deriving (Eq,Ord,Show)

instance Enum FailureReason where
    fromEnum x = case x of
        InvalidSetup -> 1
        ParsingFailed -> 2
        VariableAlreadyInUse _ -> 3
        ProcedureAlreadyDeclared _ -> 4
        CallToUnknownProcedure _ -> 5
        UseOfUnknownIdentifier _ -> 6
        EncounteredUndefined -> 7
    toEnum = undefined

instance ShowErrorComponent FailureReason where
    showErrorComponent = fromRope . render 78

data CompilationError = CompilationError (ParseErrorBundle T.Text FailureReason)
    deriving Show

instance Exception CompilationError

exitCodeFor :: CompilationError -> Int
exitCodeFor (CompilationError bundle) =
  let
    first = NonEmpty.head (bundleErrors bundle)
  in
    case first of
        TrivialError _ _ _ -> fromEnum ParsingFailed
        FancyError _ set -> case OrdSet.lookupMin set of
            Nothing -> 99
            Just fancy -> case fancy of
                ErrorCustom reason -> fromEnum reason
                _ -> 98

-- TODO upgrade this to (Doc ann) so we can get prettier error messages.

instance Render FailureReason where
    type Token FailureReason = TechniqueToken
    colourize = colourizeTechnique
    intoDocA failure = case failure of
        InvalidSetup -> "Invalid setup"
        ParsingFailed -> "FIXME" -- FIXME
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
    intoDocA (CompilationError bundle) = 
      let
        first = NonEmpty.head (bundleErrors bundle)
      in
        case first of
            TrivialError _ _ _ -> pretty (errorBundlePretty bundle)
            FancyError _ _ -> pretty (errorBundlePretty bundle)

{-
            case first of
            TrivialError _ _ _ -> pretty (errorBundlePretty bundle)
            FancyError _ set -> case OrdSet.lookupMin set of
                Nothing -> "WTF Why is this empty?"
                Just fancy -> case fancy of
                    ErrorCustom reason -> intoDocA reason
                    _ -> "WTF How did we get here?"
-}
{-|
In order to have consistently formatted "compiler" failure messages, we
jump through the hoops to use megaparsec's parse error message machinery.
-}
makeErrorBundle :: Source -> FailureReason -> ParseErrorBundle T.Text FailureReason
makeErrorBundle source failure =
  let
    fancy = ErrorCustom failure
    errors = FancyError (sourceOffset source) (OrdSet.singleton fancy) NonEmpty.:| []
    bundle = ParseErrorBundle
        { bundleErrors = errors
        , bundlePosState = PosState
            { pstateInput = sourceContents source
            , pstateOffset = 7
            , pstateSourcePos = SourcePos
                { sourceName = sourceFilename source
                , sourceLine = mkPos 3
                , sourceColumn = mkPos 2
                }
            , pstateTabWidth = mkPos 4
            , pstateLinePrefix = ""
            }
        }
  in
    bundle
