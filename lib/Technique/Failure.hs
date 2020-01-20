{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Error messages from compiling.
-}
-- I generally try to avoid modules full of (only) types but these are here
-- so the can be shared in both Technique.Translate and Technique.Builtins.
module Technique.Failure where

import Prelude hiding (lines)

import Core.System.Base
import Core.System.Pretty
import Core.Text.Rope
import Core.Text.Utilities
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as OrdSet
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (PosState(..), SourcePos(..))
import Text.Megaparsec.Error
    ( ParseError(..)
    , ErrorItem(..)
    , ParseErrorBundle(..)
    )
import Text.Megaparsec.Pos (mkPos, unPos)

import Technique.Language hiding (Label)
import Technique.Formatter

data Source = Source
    { sourceContents :: Rope
    , sourceFilename :: FilePath
    , sourceOffset :: !Offset
    }
    deriving (Eq,Ord,Show)

instance Located Source where
    locationOf = sourceOffset

instance Render Source where
    type Token Source = TechniqueToken
    colourize = colourizeTechnique
    intoDocA source = pretty (sourceFilename source) <+> pretty (sourceOffset source)

emptySource :: Source
emptySource = Source
    { sourceContents = emptyRope
    , sourceFilename = "<undefined>"
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
                [token] -> "unexpected " <> formatErrorToken token <> hardline
                _ -> emptyDoc

            ex = case expected of
                [] -> emptyDoc
                xs -> "expected " <> hsep (punctuate comma (fmap formatErrorToken xs)) <> "."
          in
            un <> ex

        VariableAlreadyInUse i -> "Variable by the name of '" <> intoDocA i <> "' already defined."
        ProcedureAlreadyDeclared i -> "Procedure by the name of '" <> intoDocA i <> "' already declared."
        CallToUnknownProcedure i -> "Call to unknown procedure '" <> intoDocA i <> "'."
        UseOfUnknownIdentifier i -> "Variable '" <> intoDocA i <> "' not in scope."
        EncounteredUndefined -> "Encountered 'undefined' marker."

formatErrorToken :: Rope -> Doc ann
formatErrorToken text = if widthRope text == 1
    then formatErrorChar text
    else pretty text

formatErrorChar :: Rope -> Doc ann
formatErrorChar text =
  let
    ch = head (fromRope text) :: Char
  in
    case ch of
        '\n' -> "newline"
        _ ->  pretty ch

{-
            let
                statement = sourceStatement source
                offset = sourceOffset source
            in
                pretty offset <> ": " <> intoDocA statement <> line <>
-}

numberOfCarots :: FailureReason -> Int
numberOfCarots reason = case reason of
    InvalidSetup -> 0
    ParsingFailed unexpected _ ->
        case unexpected of
            [token] -> widthRope token - 2
            _ -> 1
    VariableAlreadyInUse i -> widthRope (unIdentifier i)
    ProcedureAlreadyDeclared i -> widthRope (unIdentifier i)
    CallToUnknownProcedure i -> widthRope (unIdentifier i)
    UseOfUnknownIdentifier i -> widthRope (unIdentifier i)
    EncounteredUndefined -> 1

instance Render CompilationError where
    type Token CompilationError = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (CompilationError source reason) =
      let
        filename = pretty (sourceFilename source)
        contents = intoRope (sourceContents source)
        offset = sourceOffset source

-- Given an offset point where the error occured, split the input at that
-- point.

        (before,_) = splitRope offset contents
        (l,c) = calculatePositionEnd before

-- Isolate the line on which the error occured. l and c are 1-origin here,
-- so if there's only a single line (or empty file) we take that one single
-- line and then last one is also that line.

        lines = breakLines contents
        lines' = take l lines
        offending = if nullRope contents
            then emptyRope
            else last lines'

-- Now prepare for rendering. If the offending line is long trim it. Then
-- create a line with some carets which show where the problem is.

        linenum = pretty l
        colunum = pretty c
        (truncated,_) = splitRope 77 offending
        trimmed = if widthRope offending > 77 && c < 77
            then truncated <> "..."
            else offending

        padding = replicateChar (c - 1) ' '
        caroted = replicateChar (numberOfCarots reason) '^'

      in
        filename <> ":" <> linenum <> ":" <> colunum <> hardline <>
        hardline <>
        pretty trimmed <> hardline <>
        pretty padding <> pretty caroted <> hardline <>
        hardline <>
        intoDocA reason



{-|
When we get a failure in the parsing stage **megaparsec** returns a
ParseErrorBundle. Extract the first error message therein (later handle
more? Yeah nah), and convert it into something we can use.
-}
extractErrorBundle :: Source -> ParseErrorBundle T.Text Void -> CompilationError
extractErrorBundle source bundle =
  let
    errors = bundleErrors bundle
    first = NonEmpty.head errors
    (offset,unexpected,expected) = extractParseError first
    pstate = bundlePosState bundle
    srcpos = pstateSourcePos pstate

-- Do we need these? For all the examples we have seen the values of l0 and c0
-- are `1`. **megaparsec** delays calculation of line and column until
-- error rendering time. Perhaps we need to record this.

    l0 = unPos . sourceLine $ srcpos
    c0 = unPos . sourceColumn $ srcpos

    l = if l0 > 1 then error "Unexpected line balance" else 0
    c = if c0 > 1 then error "Unexpected columns balance" else 0

    reason = ParsingFailed unexpected expected

    source' = source
        { sourceOffset = offset + l + c
        }
  in
    CompilationError source' reason

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
        Tokens tokens ->                    -- tokens ~ chars
          let
            text = intoRope (NonEmpty.toList tokens)
          in case widthRope text of
            1 -> "'" <> text <> "'"
            _ -> "\"" <> text <> "\""
        Label chars ->                      -- wow. "Non-empty string"
          let
            text = intoRope (NonEmpty.toList chars)
          in
            text
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

