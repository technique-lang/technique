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
    , ErrorItem(..)
    , ParseErrorBundle(..)
    )
import Text.Megaparsec.Pos (Pos, mkPos, unPos)

import Technique.Language hiding (Label)
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
    | ParsingFailed [Rope] [Rope]
    | VariableAlreadyInUse Identifier
    | ProcedureAlreadyDeclared Identifier
    | CallToUnknownProcedure Identifier
    | UseOfUnknownIdentifier Identifier
    | EncounteredUndefined
    deriving (Eq,Ord,Show)

instance Enum FailureReason where
    fromEnum x = case x of
        InvalidSetup -> 1
        ParsingFailed _ _ -> 2
        VariableAlreadyInUse _ -> 3
        ProcedureAlreadyDeclared _ -> 4
        CallToUnknownProcedure _ -> 5
        UseOfUnknownIdentifier _ -> 6
        EncounteredUndefined -> 7
    toEnum = undefined

data CompilationError = CompilationError Source FailureReason
    deriving Show

instance Exception CompilationError

exitCodeFor :: CompilationError -> Int
exitCodeFor (CompilationError _ reason) = fromEnum reason

-- TODO upgrade this to (Doc ann) so we can get prettier error messages.

instance Render FailureReason where
    type Token FailureReason = TechniqueToken
    colourize = colourizeTechnique
    intoDocA failure = case failure of
        InvalidSetup -> "Invalid setup!"

        ParsingFailed unexpected expected ->
          let
            un = case unexpected of
                [token] -> "unexpected '" <> pretty token <> "'"
                _ -> emptyDoc

            ex = case expected of
                xs -> "expected " <> hsep (punctuate comma (fmap (enclose squote squote . pretty) xs))
                _ -> emptyDoc
          in
            un <+> ex

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
    intoDocA (CompilationError source reason) =
      let
        filename = pretty (sourceFilename source)
        contents = intoRope (sourceContents source)
        offset = sourceOffset source
        (before,after) = splitRope offset contents
        (offending,_) = splitRope 50 after          -- we really need `take` and `span` functions

        (l,c) = calculatePositionEnd before
        linenum = pretty l
        colunum = pretty c
      in
        filename <> ":" <> linenum <> ":" <> colunum <> hardline <>
        hardline <>
        pretty offending <> hardline <>
        hardline <>
        intoDocA reason



{-|
When we get a failure in the parsing stage **megaparsec** returns a
ParseErrorBundle. Extract the first error message therein (later handle
more? Yeah nah), and convert it into something we can use.
-}
extractErrorBundle :: ParseErrorBundle T.Text Void -> CompilationError
extractErrorBundle bundle =
  let
    errors = bundleErrors bundle
    first = NonEmpty.head errors
    (offset,unexpected,expected) = extractParseError first
    pstate = bundlePosState bundle
    contents = pstateInput pstate
    srcpos = pstateSourcePos pstate
    filename = sourceName srcpos

-- Do we need these? For all the examples we have seen the values of l0 and c0
-- are `1`. **megaparsec** delays calculation of line and column until
-- error rendering time. Perhaps we need to record this.

    l0 = unPos . sourceLine $ srcpos
    c0 = unPos . sourceColumn $ srcpos

    l = if l0 > 1 then error "Unexpected line balance" else 0
    c = if c0 > 1 then error "Unexpected columns balance" else 0

    reason = ParsingFailed unexpected expected

    source = Source
        { sourceContents = contents
        , sourceFilename = filename
        , sourceStatement = Blank
        , sourceOffset = offset + l + c
        }
  in
    CompilationError source reason

extractParseError :: ParseError T.Text Void -> (Int,[Rope],[Rope])
extractParseError e = case e of
    TrivialError offset unexpected0 expected0 ->
      let
        unexpected = case unexpected0 of
            Just item -> itemToRope item : []
            Nothing -> []
        expected = fmap itemToRope (OrdSet.toList expected0)
      in
        (offset,unexpected,expected)
    FancyError _ _ -> error "Unexpected parser error"

  where
    itemToRope :: ErrorItem Char -> Rope
    itemToRope item = case item of
        Tokens tokens -> intoRope (NonEmpty.toList tokens)     -- tokens ~ chars
        Label chars -> intoRope (NonEmpty.toList chars)        -- wow. "Non-empty string"
        EndOfInput -> "end of input"

{-|
This is a wrapper to put the line,column calculation into **megaparsec** terms.
-}
reachOffset2 :: Int -> PosState Rope -> (SourcePos, Rope, PosState Rope)
reachOffset2 target pstate =
  let
    input = pstateInput pstate
    (before,after) = splitRope target input
    src = pstateSourcePos pstate
    l = unPos (sourceLine src)
    c = unPos (sourceColumn src)

    (l1,c1) = calculatePositionEnd before
    l' = l + l1
    c' = c + c1

    src' = src
        { sourceLine = mkPos l'
        , sourceColumn = mkPos c'
        }
    pstate' = pstate
        { pstateSourcePos = src'
        }
  in
    (src',after,pstate')

