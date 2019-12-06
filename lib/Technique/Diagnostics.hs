{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Technique.Diagnostics where

import Core.Text.Rope
import Core.Text.Utilities
import Data.Foldable (foldl')
import Data.DList (toList)
import Data.Int (Int8)
import Data.Text.Prettyprint.Doc
    ( Doc, Pretty(pretty), viaShow, dquote, comma, punctuate, lbracket
    , rbracket, vsep, (<+>), indent, lbrace, rbrace, lparen, rparen, emptyDoc
    , line, sep, hcat, vcat, annotate
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
        annotate StepToken "Subroutine" <+> annotate ProcedureToken (pretty (procedureName proc)) <>
            line <> indent 4 (intoDocA step)

instance Render Step where
    type Token Step = TechniqueToken
    colourize = colourizeTechnique
    intoDocA step = case step of
        Known value ->
            annotate StepToken "Known" <+> intoDocA value

        Depends name ->
            annotate StepToken "Depends" <+> intoDocA name

        NoOp ->
            annotate ErrorToken "NoOp"

        Tuple steps ->
            annotate StepToken "Tuple" <+>
                lparen <+>
                commaCat steps <+>
                rparen

        Nested steps ->
            vcat (toList (fmap intoDocA steps))

        Asynchronous names substep ->
            annotate StepToken "Asynchronous" <+> commaCat names <+> "<-" <+> intoDocA substep

        Invocation attr func substep ->
          let
            i = procedureName (subroutineSource func)
          in
            annotate StepToken "Invocation" <+> intoDocA attr <+> annotate ApplicationToken (intoDocA i)

        External attr prim substep ->
          let
            i = procedureName (primitiveSource prim)
          in
            annotate StepToken "External" <+> intoDocA attr <+> annotate ApplicationToken (intoDocA i)

        Bench pairs ->      -- [(Label,Step)]
            annotate StepToken "Bench" <+>
                lbracket <+>
                foldl' f emptyDoc pairs <+>
                rbracket
          where
            f :: Doc TechniqueToken -> (Label,Step) -> Doc TechniqueToken
            f built (label,substep) =
                built <>
                dquote <> annotate LabelToken (pretty label) <> dquote <+>
                "<-" <+> intoDocA substep

instance Render Name where
    type Token Name = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Name name) = annotate VariableToken (pretty name)


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
        

