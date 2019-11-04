{-# LANGUAGE GADTs #-}

module Technique.Evaluator where

import Core.Text
import Data.UUID.Types (UUID)

import Technique.Language

data Value a = Value
    { valueKind :: Kind
    , valueInternal :: a
    }

{-|
What "kind" of type is this thing?
-}
-- this is not really the type theory sense of the word "kind". If you want
-- us to rename this to Type by all means, but please suggest a better name
-- for Technique.Language.Type. Cheers!
data Kind
    = Unit
    | Scalar
    | Table

{-
data Expression b where
    Binding :: Variable b -> Expression a -> Expression b
    Comment :: Rope -> Expression ()
    Declaration :: (a -> b) -> Expression (a -> b)
    Application :: Expression (a -> b) -> Expression a -> Expression b 
    Attribute :: Role -> Expression a -> Expression a
-}


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
-}
evaluate :: Context -> Procedure -> Value
evaluate c procedure = case procedure of
    Comment _ -> ()
    _ -> undefined

data Instance = Instance
    { instanceProcedure :: Procedure
    , instanceUniversal :: UUID
    , instancePath :: Rope -- ?
    }

instantiate :: Context -> Procedure -> Instance
instantiate context procedure = undefined
