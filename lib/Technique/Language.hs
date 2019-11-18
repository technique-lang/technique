{-# LANGUAGE OverloadedStrings #-}

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

unIdentifier :: Identifier -> Rope
unIdentifier (Identifier text) = text

-- TODO construction needs to validate internal rules for labels. No
-- newlines, perhaps.
type Label = Rope

data Attribute
    = Role Identifier
    | Place Identifier
    deriving (Show, Eq)

data Markdown
    = Markdown Rope
    deriving (Eq)

instance Show Markdown where
    show (Markdown text) = "[quote|\n" ++ fromRope text ++ "|]"

data Type
    = Type Rope
    deriving (Show, Eq)

unitType :: Type
unitType = Type "()"

data Procedure = Procedure
    { procedureName :: Identifier
    , procedureParams :: [Identifier]
    , procedureInput :: [Type]
    , procedureOutput :: [Type]
    , procedureLabel :: Maybe Markdown
    , procedureDescription :: Maybe Markdown
    , procedureBlock :: Block
    }
    deriving (Show, Eq)

emptyProcedure :: Procedure
emptyProcedure = Procedure
    { procedureName = Identifier "none"
    , procedureParams = []
    , procedureInput = [unitType]
    , procedureOutput = [unitType]
    , procedureLabel = Nothing
    , procedureDescription = Nothing
    , procedureBlock = Block []
    }

data Block = Block [Statement]
    deriving (Show, Eq)

blockStatements :: Block -> [Statement]
blockStatements (Block statements) = statements

data Statement
    = Assignment [Identifier] Expression
    | Execute Expression
    | Comment Rope
    | Declaration Procedure
    | Blank
    | Series
    deriving (Show, Eq)

data Expression
    = Application Identifier Expression     -- this had better turn out to be a procedure
    | None
    | Text Rope
    | Amount Quantity
    | Undefined
    | Object Tablet
    | Variable [Identifier]
    | Operation Operator Expression Expression
    | Grouping Expression
    | Restriction Attribute Block
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
    = WaitEither
    | WaitBoth
    | Combine
    deriving (Show, Eq)
