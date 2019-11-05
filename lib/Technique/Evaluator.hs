{-# LANGUAGE GADTs #-}

{-|
Given a Technique Procedure, transform it into an Instance that can be executed.
-}
module Technique.Evaluator where

import Core.Data
import Core.Text
import Data.UUID.Types (UUID)

import Technique.Language
import Technique.Quantity

-- Need names? Science names newly discovered creatures in Latin. I don't
-- speak Latin, but neither does anyone else so we can just make words up.
-- Yeay! (The lengths some people will go to in order to avoid qualified
-- imports is really impressive, isn't it?)
data Value
    = Unitus
    | Literali Rope
    | Quanticle Quantity
    | Tabularum [(Rope,Value)]

data Context = Context (Map Identifier Value)

data Environment = Environment (Map Identifier Value)


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
