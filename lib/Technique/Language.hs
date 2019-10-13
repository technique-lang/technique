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

-- TODO *not* Maybe! If we have two option types here then there are four
-- possibilities we need to cater for [(Nothing,Nothing), ... (Just,Just)]
-- which means there are four semantics for a given table row. Hm.

-- TODO there's an ambiguity here between the type of a table and the type
-- of an individual quantity. Which is "type"?

{-
data Value = Value
    { valueType :: Type 
    , valueLabel :: Maybe Label
    , valueData :: Maybe Quantity
    }
-}
data Role
    = Any
    | Role Rope

data Markdown = Markdown Rope

{-
data Expression b where
    Binding :: Variable b -> Expression a -> Expression b
    Comment :: Rope -> Expression ()
    Declaration :: (a -> b) -> Expression (a -> b)
    Application :: Expression (a -> b) -> Expression a -> Expression b 
    Attribute :: Role -> Expression a -> Expression a
-}


data Type = Type
    { typeName :: Rope
--  , typeInternal :: t
    }

data Procedure = Procedure
    { procedureName :: Rope
    , procedureInput :: Type
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

data Tablet = Tablet [Binding]

-- only valid Expressions are Literal and Variable. Should we enforce that
-- somewhere?
data Binding where
     Binding :: Label -> Expression -> Binding

data Operator where
    Operator :: Rope -> Operator


{-
-- aka apply
execute :: Procedure -> Value -> b
execute proc value =
  let
    body = procedureBlock proc
  in
    evaluate body

type Context = Map String Value -- ?


evaluate = undefined

evaluate :: Context -> Expression -> Value
evaluate e = case e of
    Comment _ -> ()
    _ -> undefined
-}