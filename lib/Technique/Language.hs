{-# LANGUAGE GADTs #-}

module Technique.Language where

import Core.Data.Structures
import Core.Text.Rope

import Technique.Quantity

-- TODO
data Variable
    = Constant
    | Future

data Value = Value
    { valueType :: Type
    , valueActual :: Maybe String       -- FIXME no wtf
    }

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


data Type = Type
    { typeName :: Rope
--  , typeInternal :: t
    }

data Procedure = Procedure
    { procedureName :: Rope
    , procedureInput :: Type
    , procedureOutput :: Type
    , procedureLabel :: Markdown
    , procedureDescription :: Markdown
    , procedureBlock :: Block
    }

data Block = Block [Statement]

data Statement where
    Binding :: Variable -> Expression -> Statement
    Comment :: Rope -> Statement
    Declaration :: Procedure -> Statement
    Attribute :: Role -> Block -> Statement
    Result :: Expression -> Statement

data Expression where
    Application :: Procedure -> Expression -> Expression
    Quantity :: Quantity -> Expression


-- aka apply
execute :: Procedure -> Value -> b
execute proc value =
  let
    body = procedureBlock proc
  in
    evaluate body

type Context = Map String Value -- ?

evaluate = undefined

{-
evaluate :: Context -> Expression -> Value
evaluate e = case e of
    Comment _ -> ()
    _ -> undefined
-}