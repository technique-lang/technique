{-# LANGUAGE GADTs #-}

module Technique.Language where

import Core.Text.Rope

import Technique.Quantity

-- TODO
data Name where
    Name :: Rope -> Name
    deriving (Show, Eq)

-- TODO construction needs to validate internal rules for labels. No
-- newlines, perhaps.
type Label = Rope

data Role
    = Any
    | Role Rope
    | Place Rope
    deriving (Show, Eq)

data Markdown = Markdown Rope
    deriving (Show, Eq)

data Type where
    Type :: Rope -> Type
    deriving (Show, Eq)

data Procedure = Procedure
    { procedureName :: Rope
    , procedureParams :: [Name]
    , procedureInput :: [Type]
    , procedureOutput :: Type
    , procedureLabel :: Maybe Markdown
    , procedureDescription :: Maybe Markdown
    , procedureBlock :: Block
    }
    deriving (Show, Eq)

data Block where
    Block :: [Statement] -> Block
    deriving (Show, Eq)

data Statement where
    Assignment :: Name -> Expression -> Statement
    Execute :: Expression -> Statement
    Comment :: Rope -> Statement
    Declaration :: Procedure -> Statement
    Attribute :: Role -> Block -> Statement     -- Role, Location, and ...?
    Blank :: Statement
    deriving (Show, Eq)

data Expression where
    Application :: Procedure -> Expression -> Expression
    Literal :: Quantity -> Expression
    Table :: Tablet -> Expression
    Variable :: Name -> Expression
    Operation :: Operator -> Expression -> Expression -> Expression
    Grouping :: Expression -> Expression
    deriving (Show, Eq)

data Tablet where
    Tablet :: [Binding] -> Tablet
    deriving (Show, Eq)

-- only valid Expressions are Literal and Variable. Should we enforce that
-- somewhere?
data Binding where
    Binding :: Label -> Expression -> Binding
    deriving (Show, Eq)

data Operator where
    Operator :: Rope -> Operator
    deriving (Show, Eq)