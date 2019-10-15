module Technique.Language where

import Core.Text.Rope

import Technique.Quantity

data Technique = Technique
    { techniqueVersion :: Int
    , techniqueLicense :: Rope
    , techniqueCopyright :: Maybe Rope
    , techniqueBody :: [Procedure]
    }
    deriving (Show, Eq)

-- TODO
data Name
    = Name Rope
    deriving (Show, Eq)

-- TODO construction needs to validate internal rules for labels. No
-- newlines, perhaps.
type Label = Rope

data Role
    = Any
    | Role Rope
    | Place Rope
    deriving (Show, Eq)

data Markdown
    = Markdown Rope
    deriving (Show, Eq)

data Type
    = Type Rope
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

data Block = Block [Statement]
    deriving (Show, Eq)

data Statement
    = Assignment Name Expression
    | Execute Expression
    | Comment Rope
    | Declaration Procedure
    | Attribute Role Block     -- Role, Location, and ...?
    | Blank
    deriving (Show, Eq)

data Expression
    = Application Procedure Expression
    | Literal Quantity
    | Table Tablet
    | Variable Name
    | Operation Operator Expression Expression
    | Grouping Expression
    deriving (Show, Eq)

data Tablet
    = Tablet [Binding]
    deriving (Show, Eq)

-- only valid Expressions are Literal and Variable. Should we enforce that
-- somewhere?
data Binding
    = Binding Label Expression
    deriving (Show, Eq)

data Operator
    = Operator Rope
    deriving (Show, Eq)
