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

data Environment = Environment (Map Identifier Value)

{-
data Expression b where
    Binding :: Variable b -> Expression a -> Expression b
    Comment :: Rope -> Expression ()
    Declaration :: (a -> b) -> Expression (a -> b)
    Application :: Expression (a -> b) -> Expression a -> Expression b 
    Attribute :: Role -> Expression a -> Expression a
-}

evaluate :: Environment -> Instance -> Value
evaluate env inst = undefined


functionApplication :: Procedure -> Value -> Value
functionApplication = undefined

lookupProcedure :: Environment -> Identifier -> Procedure
lookupProcedure = undefined

lookupValue :: Environment -> Identifier -> Value
lookupValue = undefined

waitEither :: Value -> Value -> Value
waitEither = undefined

waitBoth :: Value -> Value -> Value
waitBoth = undefined

combineValues :: Value -> Value -> Value
combineValues = undefined

applyRestriction :: Attribute -> Block -> Value
applyRestriction = undefined
