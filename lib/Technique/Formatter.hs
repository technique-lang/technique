{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Technique.Formatter where

import Core.Text.Rope ()
import Core.Text.Utilities
import Data.Foldable (foldl')
import Data.Text.Prettyprint.Doc
    ( Doc, Pretty(pretty), viaShow, dquote, comma, punctuate, lbracket
    , rbracket, vsep, (<+>), indent, lbrace, rbrace, emptyDoc
    , line, sep, hcat, annotate
    , unAnnotate, line', group, nest
    )
import Data.Text.Prettyprint.Doc.Render.Terminal
    ( color, colorDull, Color(..), AnsiStyle
    )

import Technique.Language
import Technique.Quantity

data TechniqueToken
    = ProcedureToken
    | TypeToken
    | SymbolToken
    | StringToken
    | QuantityToken
    | RoleToken

instance Pretty Procedure where
    pretty = unAnnotate . intoDocA

colourizeTechnique :: TechniqueToken -> AnsiStyle
colourizeTechnique token = case token of
    ProcedureToken -> color Blue
    TypeToken -> colorDull Yellow
    SymbolToken -> colorDull White
    StringToken -> colorDull Cyan
    QuantityToken -> colorDull Green
    RoleToken -> colorDull Yellow

instance Render Procedure where
    type Token Procedure = TechniqueToken
    colourize = colourizeTechnique
    intoDocA proc =
      let
        name = pretty . procedureName $ proc
        from = pretty . typeName . procedureInput $ proc
        into = pretty . typeName . procedureOutput $ proc
        block = intoDocA . procedureBlock $ proc
      in
        annotate ProcedureToken name
        <+> annotate SymbolToken ":"
        <+> annotate TypeToken from
        <+> annotate SymbolToken "->"
        <+> annotate TypeToken into
        <> block

instance Render Block where
    type Token Block = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Block statements) =
        nest 4 (
            annotate SymbolToken lbrace <>
            foldl' f emptyDoc statements
        ) <>
        line <>
        annotate SymbolToken rbrace
      where
        f :: Doc TechniqueToken -> Statement -> Doc TechniqueToken
        f built statement = built <> line <> intoDocA statement

instance Render Statement where
    type Token Statement = TechniqueToken
    colourize = colourizeTechnique
    intoDocA statement = case statement of
        (Assignment var expr) -> undefined
        (Execute expr) -> undefined
        (Comment text) -> "-- " <> pretty text  -- TODO what about multiple lines?
        (Declaration proc) -> intoDocA proc
        (Attribute role block) -> undefined
        (Result expr) -> undefined

instance Render Expression where
    type Token Expression = TechniqueToken
    colourize = colourizeTechnique
    intoDocA expr = case expr of
        Application proc subexpr -> undefined
        Literal quantity -> undefined
        Table tablet -> undefined
        Binding label subexpr -> undefined
        Evaluate variable -> undefined
