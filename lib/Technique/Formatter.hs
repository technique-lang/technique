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
    , dquotes, parens
    )
import Data.Text.Prettyprint.Doc.Render.Terminal
    ( color, colorDull, Color(..), AnsiStyle, bold
    )

import Technique.Language
import Technique.Quantity

data TechniqueToken
    = ProcedureToken
    | TypeToken
    | SymbolToken
    | VariableToken
    | ApplicationToken
    | LabelToken
    | StringToken
    | QuantityToken
    | RoleToken
    | ErrorToken

instance Pretty Procedure where
    pretty = unAnnotate . intoDocA

-- no use of white, suggesting colours
colourizeTechnique :: TechniqueToken -> AnsiStyle
colourizeTechnique token = case token of
    ProcedureToken -> color Blue
    TypeToken -> colorDull Yellow
    SymbolToken -> colorDull Cyan <> bold
    VariableToken -> color Cyan
    ApplicationToken -> color Blue <> bold
    LabelToken -> color Yellow
    StringToken -> color Green <> bold
    QuantityToken -> color Magenta
    RoleToken -> colorDull Yellow
    ErrorToken -> color Red <> bold

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
        line <>
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
        (Assignment var expr) ->
            intoDocA var <+> annotate SymbolToken "=" <+> intoDocA expr
        (Execute expr) ->
            intoDocA expr
        (Comment text) ->
            "-- " <> pretty text  -- TODO what about multiple lines?
        (Declaration proc) ->
            intoDocA proc
        (Attribute role block) ->
            intoDocA role <>
            line <>
            intoDocA block        -- TODO some nesting?
        (Result expr) ->
            intoDocA expr

instance Render Role where
    type Token Role = TechniqueToken
    colourize = colourizeTechnique
    intoDocA role =  case role of
        Any -> annotate RoleToken "$*"
        Role name -> annotate RoleToken ("$" <> pretty name)

instance Render Expression where
    type Token Expression = TechniqueToken
    colourize = colourizeTechnique
    intoDocA expr = case expr of
        Application proc subexpr ->
          let
            name = pretty . procedureName $ proc
          in
            annotate ApplicationToken name <+> intoDocA subexpr
        Literal qty ->
            intoDocA qty
        Table tablet ->
            intoDocA tablet
        Binding label subexpr ->
            annotate SymbolToken (dquotes (annotate LabelToken (pretty label))) <+>
            annotate SymbolToken "~" <+>
            intoDocA subexpr
        Evaluate var ->
            intoDocA var

instance Render Variable where
    type Token Variable = TechniqueToken
    colourize = colourizeTechnique
    intoDocA var =
      let
        name = pretty . variableName $ var
      in
        -- TODO different highlighting for variable names?
        annotate VariableToken name

instance Render Quantity where
    type Token Quantity = TechniqueToken
    colourize = colourizeTechnique
    intoDocA qty = case qty of
        None ->
            annotate ErrorToken "?"
        Number i ->
            annotate QuantityToken (pretty i)
        Quantity i unit ->
            annotate SymbolToken (parens (annotate QuantityToken (pretty i <+> pretty (unitSymbol unit))))
        Text text ->
            annotate SymbolToken (dquotes (annotate StringToken (pretty text)))

instance Render Tablet where
    type Token Tablet = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Tablet exprs) =
        nest 4 (
            annotate SymbolToken lbracket <>
            foldl' g emptyDoc exprs
        ) <>
        line <>
        annotate SymbolToken rbracket
      where
        g :: Doc TechniqueToken -> Expression -> Doc TechniqueToken
        g built expr@(Binding _ _) = built <> line <> intoDocA expr
        g _ _ = error "Only Binding is valid"
