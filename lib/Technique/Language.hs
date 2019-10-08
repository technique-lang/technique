{-# LANGUAGE GADTs #-}

module Technique.Language where

import Core.Data.Structures
import Core.Text.Rope

data Variable b
    = Constant b
    | Future b

data Role
    = None
    | Any
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


data Type t = Type
    { typeName :: Rope
    , typeInternal :: t
    }

data Procedure a b = Expression
    { procedureName :: Rope
    , procedureInput :: Type a
    , procedureOutput :: Type b
    , procedureLabel :: Markdown
    , procedureDescription :: Markdown
    , procedureBlock :: Statement b
    }

data Block b = Block [Statement b]      -- wrong, because they have different types along the way

data Statement b where
    Binding :: Variable b -> Expression a -> Statement b
    Comment :: Rope -> Statement ()
    Attribute :: Role -> Block b -> Statement b

data Expression b where
    Application :: Expression (a -> b) -> Expression a -> Expression b
    Value :: Quantity b -> Expression b
    Declaration :: Procedure a b -> Statement b


-- aka apply
execute :: Procedure a b -> Value a -> b
execute proc value =
  let
    body = procedureBody proc
  in
    evaluate body

type Context = Map String Value -- ?

evaluate :: Context -> Expression -> Value
evaluate e = case e of
    Comment _ -> ()
    _ -> undefined
