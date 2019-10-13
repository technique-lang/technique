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
    , rbracket, vsep, (<+>), indent, lbrace, rbrace, lparen, rparen, emptyDoc
    , line, sep, hcat, annotate
    , unAnnotate, line', group, nest, concatWith, surround
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
    ProcedureToken -> colorDull Blue <> bold
    TypeToken -> colorDull Yellow
    SymbolToken -> colorDull Cyan <> bold
    VariableToken -> color Cyan
    ApplicationToken -> color Blue <> bold
    LabelToken -> color Green <> bold
    StringToken -> color Green <> bold
    QuantityToken -> color Magenta <> bold
    RoleToken -> colorDull Yellow
    ErrorToken -> color Red <> bold

instance Render Procedure where
    type Token Procedure = TechniqueToken
    colourize = colourizeTechnique
    intoDocA proc =
      let
        name = pretty . procedureName $ proc
        params = concatWith (surround (annotate SymbolToken comma)) (fmap intoDocA (procedureParams proc))
        from = pretty . typeName . procedureInput $ proc
        into = pretty . typeName . procedureOutput $ proc
        block = intoDocA . procedureBlock $ proc
      in
        annotate ProcedureToken name <+>
        params <+>
        annotate SymbolToken ":" <+>
        annotate TypeToken from <+>
        annotate SymbolToken "->" <+>
        annotate TypeToken into <>
        block

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
        Assignment var expr ->
            intoDocA var <+> annotate SymbolToken "=" <+> intoDocA expr
        Execute expr ->
            intoDocA expr
        Comment text ->
            "-- " <> pretty text  -- TODO what about multiple lines?
        Declaration proc ->
            intoDocA proc
        Attribute role block ->
            intoDocA role <>
            line <>
            intoDocA block        -- TODO some nesting?
        Blank -> emptyDoc

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
        Variable var ->
            intoDocA var
        Operation operator subexpr1 subexpr2 ->
            intoDocA subexpr1 <+> intoDocA operator <+> intoDocA subexpr2
        Grouping subexpr ->
            annotate SymbolToken lparen <>
            intoDocA subexpr <>
            annotate SymbolToken rparen

instance Render Name where
    type Token Name = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Name name) =
        annotate VariableToken (pretty name)

instance Render Quantity where
    type Token Quantity = TechniqueToken
    colourize = colourizeTechnique
    intoDocA qty = case qty of
        None ->
            annotate ErrorToken "?"
        Number i ->
            annotate QuantityToken (pretty i)
        Quantity i unit ->
            annotate QuantityToken (pretty i <+> pretty (unitSymbol unit))
        Text text ->
            annotate SymbolToken dquote <>
            annotate StringToken (pretty text) <>
            annotate SymbolToken dquote


instance Render Tablet where
    type Token Tablet = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Tablet bindings) =
        nest 4 (
            annotate SymbolToken lbracket <>
            foldl' g emptyDoc bindings
        ) <>
        line <>
        annotate SymbolToken rbracket
      where
        g :: Doc TechniqueToken -> Binding -> Doc TechniqueToken
        g built binding = built <> line <> intoDocA binding

-- the annotation for the label duplicates the code Quantity's Text
-- constructor, but for the LabelToken token. This distinction may not be
-- necessary (at present we have the same colouring for both).
instance Render Binding where
    type Token Binding = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Binding label subexpr) =
            annotate SymbolToken dquote <>
            annotate LabelToken (pretty label) <>
            annotate SymbolToken dquote <+>
            annotate SymbolToken "~" <+>
            intoDocA subexpr

instance Render Operator where
    type Token Operator = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Operator symbol) =
        annotate SymbolToken (pretty symbol)
