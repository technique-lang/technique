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
    | Parametri [Value]         -- TODO hmm

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

evaluate :: Environment -> Expression -> Value
evaluate env expr' = case expr' of
    Application i expr -> functionApplication (lookupProcedure env i) (evaluate env expr)    -- lookup needs to return a function?
    None    -> Unitus
    Text text -> Literali text
    Amount qty -> Quanticle qty
    Undefined -> error "?!?" -- not error but "hole, stop here"
    Object (Tablet bindings) -> Tabularum (fmap (\(Binding label expr) -> (label,evaluate env expr)) bindings)
    Variable is -> Parametri (fmap (lookupValue env) is)    -- TODO hmm
    Operation op expr1 expr2 -> case op of
        WaitEither  -> waitEither (evaluate env expr1) (evaluate env expr2)
        WaitBoth    -> waitBoth (evaluate env expr1) (evaluate env expr2)
        Combine     -> combineValues (evaluate env expr1) (evaluate env expr2)
    Grouping expr -> evaluate env expr
    Restriction attr block -> applyRestriction attr block


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

data Instance = Instance
    { instanceProcedure :: Procedure
    , instanceUniversal :: UUID
    , instancePath :: Rope -- ?
    }

newtype Name = Name Rope -- ???

{-|
Names. Always needing names. These ones are taken from a suggestion by Oleg
Kiselyov on page 23 of his course "Typed Tagless Final Interpreters" that
the constructors of a simply typed lambda calculus in this style could be
considered a "minimal intuitionistic logic" which is absolutely fabulous.
-}
data Step
    = Axiom Value                       -- literals
    | Hypothesis Name                   -- reference to a hypothesis denoted by a variable
    | Introduction Step                 -- implication introduction, ie lambda, ie assignment?
    | Elimination Instance Step         -- implication elimination, ie function application

                                        -- assumption axiom?
                                        -- weakening?

instantiate :: Context -> Procedure -> Instance
instantiate context procedure = Instance
    { instanceProcedure = procedure
    , instanceUniversal = undefined -- ** we need IO here now!!! **
    , instancePath = undefined
    }
