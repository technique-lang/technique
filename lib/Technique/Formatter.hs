{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Technique.Formatter where

import Core.Text.Rope
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
    | OperatorToken
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
    OperatorToken -> colorDull Yellow <> bold
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
        name = intoDocA . procedureName $ proc
        params = commaCat . procedureParams $ proc
        from = commaCat . procedureInput $ proc
        into = intoDocA . procedureOutput $ proc
        block = intoDocA . procedureBlock $ proc
      in
        annotate ProcedureToken name <+>
        params <+>
        annotate SymbolToken ":" <+>
        annotate TypeToken from <+>
        annotate SymbolToken "->" <+>
        annotate TypeToken into <>
        line <> block

{-|
punctuate a list with commas annotated with Symbol highlighting.
-}
commaCat :: (Render a, Token a ~ TechniqueToken)  => [a] -> Doc (Token a)
commaCat = hcat . punctuate (annotate SymbolToken comma) . fmap intoDocA

instance Render Type where
    type Token Type = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Type name) = annotate TypeToken (pretty name)

instance Render Block where
    type Token Block = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Block statements) =
        nest 4 (
            annotate SymbolToken lbrace <>
            go statements
        ) <>
        line <>
        annotate SymbolToken rbrace
      where
        go :: [Statement] -> Doc TechniqueToken
        go [] = emptyDoc
        go (Series:x1:xs) = intoDocA Series <> intoDocA x1 <> go xs
        go (x:xs) = line <> intoDocA x <> go xs

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
        Blank ->
            emptyDoc
        Series ->
            annotate SymbolToken " ; "

instance Render Role where
    type Token Role = TechniqueToken
    colourize = colourizeTechnique
    intoDocA role =  case role of
        Any -> annotate RoleToken "@*"
        Role name -> annotate RoleToken ("@" <> pretty name)
        Place name -> annotate RoleToken ("#" <> pretty name)

instance Render Expression where
    type Token Expression = TechniqueToken
    colourize = colourizeTechnique
    intoDocA expr = case expr of
        Application name subexpr ->
            annotate ApplicationToken (intoDocA name) <+> intoDocA subexpr
        Literal qty ->
            intoDocA qty
        Object tablet ->
            intoDocA tablet
        Variable var ->
            intoDocA var
        Operation operator subexpr1 subexpr2 ->
            intoDocA subexpr1 <+> intoDocA operator <+> intoDocA subexpr2
        Grouping subexpr ->
            annotate SymbolToken lparen <>
            intoDocA subexpr <>
            annotate SymbolToken rparen

instance Render Identifier where
    type Token Identifier = TechniqueToken
    colourize = colourizeTechnique
    intoDocA (Identifier name) =
        annotate VariableToken (pretty name)

instance Render Quantity where
    type Token Quantity = TechniqueToken
    colourize = colourizeTechnique
    intoDocA qty = case qty of
        None ->
            annotate SymbolToken ("()")
        Undefined ->
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
    intoDocA operator =
        annotate OperatorToken $ case operator of
            WaitBoth ->     pretty '&'
            WaitEither ->   pretty '|'
            Combine ->      pretty '+'
