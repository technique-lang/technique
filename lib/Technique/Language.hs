{-# LANGUAGE GADTs #-}

module Technique.Language where

import Core.Text.Rope

import Technique.Quantity

-- TODO
data Name where
    Name :: Rope -> Name

-- TODO construction needs to validate internal rules for labels. No
-- newlines, perhaps.
type Label = Rope

data Role
    = Any
    | Role Rope
    | Place Rope

data Markdown = Markdown Rope

data Type where
    Type :: Rope -> Type

data Procedure = Procedure
    { procedureName :: Rope
    , procedureParams :: [Name]
    , procedureInput :: [Type]
    , procedureOutput :: Type
    , procedureLabel :: Maybe Markdown
    , procedureDescription :: Maybe Markdown
    , procedureBlock :: Block
    }

data Block = Block [Statement]

data Statement where
    Assignment :: Name -> Expression -> Statement
    Execute :: Expression -> Statement
    Comment :: Rope -> Statement
    Declaration :: Procedure -> Statement
    Attribute :: Role -> Block -> Statement     -- Role, Location, and ...?
    Blank :: Statement

data Expression where
    Application :: Procedure -> Expression -> Expression
    Literal :: Quantity -> Expression
    Table :: Tablet -> Expression
    Variable :: Name -> Expression
    Operation :: Operator -> Expression -> Expression -> Expression
    Grouping :: Expression -> Expression

data Tablet = Tablet [Binding]

-- only valid Expressions are Literal and Variable. Should we enforce that
-- somewhere?
data Binding where
     Binding :: Label -> Expression -> Binding

data Operator where
    Operator :: Rope -> Operator
