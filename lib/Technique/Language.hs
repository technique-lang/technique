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
data Identifier
    = Identifier Rope
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
    { procedureName :: Identifier
    , procedureParams :: [Identifier]
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
    = Assignment Identifier Expression
    | Execute Expression
    | Comment Rope
    | Declaration Procedure
    | Attribute Role Block     -- Role, Location, and ...?
    | Blank
    | Series
    deriving (Show, Eq)

data Expression
    = Application Identifier Expression     -- this had better turn out to be a procedure
    | Literal Quantity
    | Table Tablet
    | Variable Identifier
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
