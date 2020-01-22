{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Technique.Language where

import Core.Text.Rope
import Core.Data.Structures (Key)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Technique.Quantity

data Technique = Technique
    { techniqueVersion :: Int
    , techniqueLicense :: Rope
    , techniqueCopyright :: Maybe Rope
    , techniqueBody :: [Procedure]
    }
    deriving (Show, Eq)

emptyTechnique :: Technique
emptyTechnique = Technique
    { techniqueVersion = 0
    , techniqueLicense = emptyRope
    , techniqueCopyright = Nothing
    , techniqueBody = []
    }

-- TODO
data Identifier
    = Identifier Rope
    deriving (Show, Eq, Ord, Generic, Hashable)

unIdentifier :: Identifier -> Rope
unIdentifier (Identifier text) = text
{-# INLINE unIdentifier #-}

instance Key Identifier

-- TODO construction needs to validate internal rules for labels. No
-- newlines, perhaps.
newtype Label = Label Rope
    deriving (Show, Eq, Ord)

data Attribute
    = Role Identifier
    | Place Identifier
    | Inherit
    deriving (Show, Eq, Ord)

{-
    | Anyone
    | Anywhere
-}

data Markdown
    = Markdown Rope
    deriving (Eq, Ord)

instance Show Markdown where
    show (Markdown text) = "[quote|\n" ++ fromRope text ++ "|]"

data Type
    = Type Rope
    deriving (Show, Eq, Ord)

unitType :: Type
unitType = Type "()"

data Procedure = Procedure
    { procedureOffset :: Offset
    , procedureName :: Identifier
    , procedureParams :: [Identifier]
    , procedureInput :: [Type]
    , procedureOutput :: [Type]
    , procedureTitle :: Maybe Markdown
    , procedureDescription :: Maybe Markdown
    , procedureBlock :: Block
    }
    deriving (Show, Eq, Ord)

emptyProcedure :: Procedure
emptyProcedure = Procedure
    { procedureOffset = -1
    , procedureName = Identifier "none"
    , procedureParams = []
    , procedureInput = [unitType]
    , procedureOutput = [unitType]
    , procedureTitle = Nothing
    , procedureDescription = Nothing
    , procedureBlock = Block []
    }

data Block = Block [Statement]
    deriving (Show, Eq, Ord)

type Offset = Int

class Located a where
    locationOf :: a -> Offset

instance Located Procedure where
    locationOf = procedureOffset

data Statement
    = Assignment Offset [Identifier] Expression
    | Execute Offset Expression
    | Comment Offset Rope
    | Declaration Offset Procedure
    | Blank Offset
    | Series Offset
    deriving (Show, Ord, Eq)

instance Located Statement where
    locationOf statement = case statement of
        Assignment offset _ _ -> offset
        Execute offset _ -> offset
        Comment offset _ -> offset
        Declaration offset _ -> offset
        Blank offset -> offset
        Series offset -> offset

data Expression
    = Application Offset Identifier Expression     -- this had better turn out to be a procedure
    | None Offset
    | Text Offset Rope
    | Amount Offset Quantity
    | Undefined Offset
    | Object Offset Tablet
    | Variable Offset [Identifier]
    | Operation Offset Operator Expression Expression
    | Grouping Offset Expression
    | Restriction Offset Attribute Block
    deriving (Show, Ord, Eq)

instance Located Expression where
    locationOf expr = case expr of
        Application offset _ _ -> offset
        None offset -> offset
        Text offset _ -> offset
        Amount offset _ -> offset
        Undefined offset -> offset
        Object offset _ -> offset
        Variable offset _ -> offset
        Operation offset _ _ _ -> offset
        Grouping offset _ -> offset
        Restriction offset _ _ -> offset

data Tablet
    = Tablet [Binding]
    deriving (Show, Ord, Eq)

-- only valid Expressions are Literal and Variable. Should we enforce that
-- somewhere?
data Binding
    = Binding Label Expression
    deriving (Show, Eq, Ord)

data Operator
    = WaitEither
    | WaitBoth
    | Combine
    deriving (Show, Eq, Ord)
