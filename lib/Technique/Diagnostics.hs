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

instance Render Subroutine where
    type Token Subroutine = TechniqueToken
    colourize = colourizeTechnique
    intoDocA func =
      let
        proc = subroutineSource func
        step = subroutineSteps func
      in
        annotate StepToken "Subroutine" <+> annotate ProcedureToken (pretty (procedureName proc))

instance Render Step where
    type Token Step = TechniqueToken
    colourize = colourizeTechnique
    intoDocA step = case step of
        Known value ->
            annotate StepToken "Known" <+> intoDocA value
        Depends (Name name) ->
            annotate StepToken "Depends" <+> annotate VariableToken (pretty name)
        Tuple steps ->
            annotate StepToken "Tuple" <+> commaCat steps
        _ ->
            undefined

instance Render Value where
    type Token Value = TechniqueToken
    colourize = colourizeTechnique
    intoDocA value = case value of
        Unitus ->
            annotate QuantityToken "()"
        Literali text ->
            annotate SymbolToken dquote <>
            annotate StringToken (pretty text) <>
            annotate SymbolToken dquote
        Quanticle qty ->
            intoDocA qty
        _ ->
            undefined
        

