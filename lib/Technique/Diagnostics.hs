{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Technique.Diagnostics where

import Core.System.Pretty
import Core.Text.Rope
import Core.Text.Utilities
import Data.DList (toList)
import Data.Foldable (foldl')
import Data.Int (Int8)
import Technique.Formatter -- already have lots of useful definitions
import Technique.Internal
import Technique.Language

instance Render Function where
  type Token Function = TechniqueToken
  colourize = colourizeTechnique
  intoDocA func =
    nest
      3
      ( " ↘ "
          <> ( case func of
                 Unresolved i ->
                   annotate ErrorToken "Unresolved" <+> annotate ProcedureToken (pretty (unIdentifier i)) <> line
                 Subroutine proc step ->
                   annotate StepToken "Subroutine" <+> annotate ProcedureToken (pretty (procedureName proc))
                     <> line
                     <> (nest 3 (" ↘ " <> intoDocA step))
                 Primitive proc action ->
                   annotate StepToken "Primitive" <+> annotate ProcedureToken (pretty (procedureName proc))
                     <> line
                     <> " ↘ <primitive>"
             )
      )

instance Render Step where
  type Token Step = TechniqueToken
  colourize = colourizeTechnique
  intoDocA step = case step of
    Known _ value ->
      annotate StepToken "Known" <+> intoDocA value
    Depends _ name ->
      annotate StepToken "Depends" <+> intoDocA name
    NoOp ->
      annotate ErrorToken "NoOp"
    Tuple _ steps ->
      annotate StepToken "Tuple"
        <+> lparen
        <+> hsep (punctuate comma (fmap intoDocA steps))
        <+> rparen
    Nested _ steps ->
      vcat (toList (fmap intoDocA steps))
    Asynchronous _ names substep ->
      annotate StepToken "Asynch" <+> commaCat names <+> "◀-" <+> intoDocA substep
    Invocation _ attr func substep ->
      let i = functionName func
       in annotate StepToken "Invoke" <+> intoDocA attr <+> annotate ApplicationToken (intoDocA i)
            <> line
            <> nest 3 (" ↘ " <> intoDocA substep)
    Bench _ pairs ->
      -- [(Label,Step)]
      annotate StepToken "Bench"
        <> line
        <> "   "
        <> hang
          2
          ( lbracket
              <+> vsep (punctuate comma bindings)
          )
          <+> rbracket
      where
        bindings = fmap f pairs
        f :: (Label, Step) -> Doc TechniqueToken
        f (label, substep) =
          intoDocA label <+> "◀-" <+> intoDocA substep

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
      annotate SymbolToken dquote
        <> annotate StringToken (pretty text)
        <> annotate SymbolToken dquote
    Quanticle qty ->
      intoDocA qty
    _ ->
      undefined
