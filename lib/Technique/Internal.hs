{-# LANGUAGE GADTs #-}

{-|
Builing blocks for the translation stage of the compiler.
-}
-- I generally try to avoid modules full of (only) types but these are here
-- so the can be shared in both Technique.Translate and Technique.Builtins.
module Technique.Internal where

import Core.Data
import Core.Text

import Technique.Language
import Technique.Quantity

{-|
Environment in the type-theory sense of the word: the map(s) between names
and their bindings.
-}
data Environment = Environment
    { environmentVariables :: Map Identifier Name
    , environmentFunctions :: Map Identifier Procedure
    }

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
    , subroutineRole :: Attribute
    , subroutineSteps :: [Step]
    }

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

newtype Name = Name Rope -- ??? upgrade to named IVar := Promise ???

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
    = Known Value                       -- literals ("axioms")
    | Depends Name                      -- block waiting on a value ("reference to a hypothesis denoted by a variable")
    | Asynchronous [Name] Step          -- assignment (ie lambda, "implication introduction"
    | Invocation Subroutine Step        -- function application ("implication elimination") on a [sub] Procedure
    | External Primitive Step           -- same, but calling a primative builtin.
    | Tuple [Step]

                                        -- assumption axiom?
                                        -- weakening?

