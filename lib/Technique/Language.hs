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

unLabel :: Label -> Rope
unLabel (Label text) = text
{-# INLINE unLabel #-}

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
        Assignment o _ _ -> o
        Execute o _ -> o
        Comment o _ -> o
        Declaration o _ -> o
        Blank o -> o
        Series o -> o

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
        Application o _ _ -> o
        None o -> o
        Text o _ -> o
        Amount o _ -> o
        Undefined o -> o
        Object o _ -> o
        Variable o _ -> o
        Operation o _ _ _ -> o
        Grouping o _ -> o
        Restriction o _ _ -> o

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
