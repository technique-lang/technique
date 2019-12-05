{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Technique.Diagnostics where

import Core.Text.Rope
import Core.Text.Utilities
import Data.Foldable (foldl')
import Data.Int (Int8)
import Data.Text.Prettyprint.Doc
    ( Doc, Pretty(pretty), viaShow, dquote, comma, punctuate, lbracket
    , rbracket, vsep, (<+>), indent, lbrace, rbrace, lparen, rparen, emptyDoc
    , line, sep, hcat, annotate
    , unAnnotate, line', group, nest, concatWith, surround
    )
import Data.Text.Prettyprint.Doc.Render.Terminal
    ( color, colorDull, Color(..), AnsiStyle, bold
    )

import Technique.Language
import Technique.Formatter  -- already have lots of useful definitions
import Technique.Internal

data DiagnosticToken
    = StepToken
    | PunctuationToken
    | NameToken
    | SubroutineToken
    | PrimitiveToken
    | ValueToken

colourizeDiagnostic :: DiagnosticToken -> AnsiStyle
colourizeDiagnostic token = case token of
    StepToken -> colorDull Yellow
    PunctuationToken -> colorDull White
    NameToken -> colourizeTechnique VariableToken
    SubroutineToken -> colourizeTechnique ProcedureToken
    PrimitiveToken  -> color White <> bold
    ValueToken  -> colourizeTechnique QuantityToken

instance Render Subroutine where
    type Token Subroutine = DiagnosticToken
    colourize = colourizeDiagnostic
    intoDocA func =
      let
        proc = subroutineSource func
        step = subroutineSteps func
      in
        annotate SubroutineToken (pretty (procedureName proc))

instance Render Step where
    type Token Step = DiagnosticToken
    colourize = colourizeDiagnostic
    intoDocA step = case step of
        Known value ->
            annotate StepToken "Known" <+> intoDocA value
        Depends (Name name) ->
            annotate StepToken "Depends" <+> annotate NameToken (pretty name)
        Tuple steps ->
            annotate StepToken "Tuple" <+> commaCat2 steps
        _ ->
            undefined

instance Render Value where
    type Token Value = DiagnosticToken
    colourize = colourizeDiagnostic
    intoDocA value = case value of
        Unitus ->
            annotate ValueToken "()"
        Literali text ->
            annotate PunctuationToken dquote <>
            annotate ValueToken (pretty text) <>
            annotate PunctuationToken dquote
        Quanticle qty ->
            annotate ValueToken (pretty qty)
        _ ->
            undefined
        

{-|
punctuate a list with commas annotated with Symbol highlighting.
-}
-- shame we can't share this code with the original one in Technique.Formatter
commaCat2 :: (Render a, Token a ~ DiagnosticToken)  => [a] -> Doc (Token a)
commaCat2 = hcat . punctuate (annotate PunctuationToken comma) . fmap (annotate NameToken . intoDocA)

