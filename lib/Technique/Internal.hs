{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Builing blocks for the translation stage of the compiler.
-}
-- I generally try to avoid modules full of (only) types but these are here
-- so the can be shared in both Technique.Translate and Technique.Builtins.
module Technique.Internal where

import Core.Text
import Data.DList

import Technique.Language
import Technique.Quantity

-- FIXME ??? upgrade to named IVar
newtype Promise = Promise Value

{-|
The resolved value of eiter a literal or function applicaiton, either as
that literal, the expression, or as the result of waiting on the variable
it was assigned to.

-}
-- Need names? Science names newly discovered creatures in Latin. I don't
-- speak Latin, but neither does anyone else so we can just make words up.
-- Yeay! (The lengths some people will go to in order to avoid qualified
-- imports is really impressive, isn't it?)
data Value
    = Unitus
    | Literali Rope
    | Quanticle Quantity
    | Tabularum [(Rope,Value)]
    | Parametriq [Value]
    deriving (Eq,Show)

{-|
The internal representation of a Procedure, with ambiguities resolved.
-}
-- Didn't want to call this "function" because that means something in
-- functional programming and in programming language theory and this isn't
-- it. Other alternatives considered include Instance (the original name,
-- but we've reserved that to be used when instantiating a procedure at
-- runtime), Representation, and Internal. Subroutine is ok.
data Subroutine = Subroutine
    { subroutineSource :: Procedure
    , subroutineSteps :: Step
    }
    deriving (Eq,Show)

{-|
Procedures which are actually fundamental [in the context of the domain
specific language] represented by builtin IO actions.
-}
-- we use primativeSource :: Procedure so that we can duplicate the logic
-- when we are doing diagnostics. Suggests a typeclass?
{-
class Function a where
    functionSource :: a -> Procedure
-}
data Primitive = Primitive
    { primitiveSource :: Procedure
    , primitiveAction :: Step -> IO Value
    }

instance Show Primitive where
    show prim =
        let
            name = procedureName (primitiveSource prim)
        in
            show name

-- this is weak, but we can't compare functions so if the Procedures are
-- the same assume the Primitives are.
instance Eq Primitive where
    (==) p1 p2 = (primitiveSource p1) == (primitiveSource p2)

newtype Name = Name Rope -- ??? upgrade to named IVar := Promise ???
    deriving (Eq,Show)

{-|
Names. Always needing names. These ones are from original work when we
envisioned technique as a shallow embedding of a domain specific language
implemented in Haskell. Comments describing constructors are taken from a
suggestion by Oleg Kiselyov on page 23 of his course "Typed Tagless Final
Interpreters" that the constructors of a simply typed lambda calculus in
this style could be considered a "minimal intuitionistic logic" which is
absolutely fabulous.
-}
-- while it probably would work to put an Asynchronous into a Tuple list,
-- it's not valid from the point of view of the surface language syntax.
data Step
    = Known Value                           -- literals ("axioms")
    | Bench [(Label,Step)]
    | Depends Name                          -- block waiting on a value ("reference to a hypothesis denoted by a variable")
    | NoOp
    | Tuple [Step]
    | Asynchronous [Name] Step              -- assignment (ie lambda, "implication introduction"
    | Invocation Attribute Subroutine Step  -- function application ("implication elimination") on a [sub] Procedure
    | External Attribute Primitive Step     -- same, but calling a primative builtin.
    | Nested (DList Step)
                                            -- assumption axiom?
                                            -- weakening?
    deriving (Eq,Show)

instance Semigroup Step where
    (<>) = mappend

instance Monoid Step where
    mempty = NoOp
    mappend NoOp s2 = s2
    mappend s1 NoOp = s1
    mappend (Nested list1) (Nested list2) = Nested (append list1 list2)
    mappend (Nested list1) s2 = Nested (snoc list1 s2)
    mappend s1 (Nested list2) = Nested (cons s1 list2)
    mappend s1 s2 = Nested (cons s1 (singleton s2))
