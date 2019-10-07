{-# LANGUAGE GADTs #-}

module Technique.Language where

import Core.Text.Rope

data Variable a
    = Constant a
    | Future a

data Role = Role Rope

data Markdown = Markdown Rope

data Expression a where
    Bind :: Variable a -> Expression a -> Expression a
    Comment :: Rope -> Expression ()
    Declaration :: (a -> b) -> Expression (a -> b)
    Application :: Expression (a -> b) -> Expression a -> Expression b 
    Attribute :: Role -> Expression a -> Expression a

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
    , procedureBody :: Expression b
    }

evaluate :: Expression a -> a
evaluate e = case e of
    Comment _ -> ()
    _ -> undefined
