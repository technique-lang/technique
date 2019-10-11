{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Technique.Formatter where

import Core.Text.Rope
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
      in
        annotate ProcedureToken name
        <+> annotate SymbolToken ":"
        <+> annotate TypeToken from
        <+> annotate SymbolToken "->"
        <+> annotate TypeToken into
