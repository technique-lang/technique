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
import qualified Data.List as List

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
    | Tabularum [(Label,Value)]
    | Parametriq [Value]
    deriving (Eq,Show)

{-|
The internal representation of a Procedure, with ambiguities resolved.

We landed on Subroutine as the name of translated user-defined Procedure.

Procedures which are actually fundamental [in the context of the domain
specific language] represented by builtin IO actions which we call
Primatives.

The first constructor, Unresolved, is for the first stage pass through the
translate phase when we are still accumulating definitions, thereby
allowing for the forward use of not-yet-defiend procedures that will be
encountered in the same scope.
-}
-- Didn't want to call this "function" because that means something in
-- functional programming and in programming language theory and this isn't
-- it. Other alternatives considered include Instance (the original name,
-- but we've reserved that to be used when instantiating a procedure at
-- runtime), Representation, and Internal. Subroutine is ok.
data Function
    = Unresolved Identifier
    | Subroutine Procedure Step
    | Primitive Procedure (Step -> IO Value)

functionName :: Function -> Identifier
functionName func = case func of
    Unresolved name -> name
    Subroutine proc _ -> procedureName proc
    Primitive prim _ -> procedureName prim

instance Show Function where
    show func =
      let
        name = fromRope (unIdentifier (functionName func))
      in case func of
        Unresolved _ -> "Unresolved \"" ++ name ++ "\""
        Subroutine _ step -> "Subroutine \"" ++ name ++ "\": " ++ show step
        Primitive _ _ -> "Primitive \"" ++ name ++ "\""

instance Eq Function where
    (==) f1 f2 = case f1 of
        Unresolved i1 -> case f2 of
            Unresolved i2 -> i1 == i2
            _ -> False

        Subroutine proc1 step1 -> case f2 of
            Subroutine proc2 step2 -> proc1 == proc2 && step1 == step2
            _ -> False

-- this is weak, but we can't compare Haskell functions for equality so if
-- the Procedures are the same then we assume the Primitives are.

        Primitive proc1 _ -> case f2 of
            Primitive proc2 _ -> proc1 == proc2
            _ -> False

{-|
Fully resolved abstract syntax tree ready for evaluation.
-}
newtype Executable = Executable [Function]

entryPoint :: Executable -> Maybe Function
entryPoint (Executable funcs) = fmap fst (List.uncons funcs)

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
    = Known Offset Value                           -- literals ("axioms")
    | Bench Offset [(Label,Step)]
    | Depends Offset Name                          -- block waiting on a value ("reference to a hypothesis denoted by a variable")
    | NoOp
    | Tuple Offset [Step]
    | Asynchronous Offset [Name] Step              -- assignment (ie lambda, "implication introduction"
    | Invocation Offset Attribute Function Step    -- function application ("implication elimination") on a [sub] Procedure
    | Nested Offset (DList Step)
                                            -- assumption axiom?
                                            -- weakening?
    deriving (Eq,Show)

instance Located Step where
    locationOf step = case step of
        Known o _ -> o
        Bench o _ -> o
        Depends o _ -> o
        NoOp -> -2
        Tuple o _ -> o
        Asynchronous o _ _ -> o
        Invocation o _ _ _ -> o
        Nested o _ -> o

instance Semigroup Step where
    (<>) = mappend

instance Monoid Step where
    mempty = NoOp
    mappend NoOp s2 = s2
    mappend s1 NoOp = s1
    mappend (Nested o1 list1) (Nested _ list2) = Nested o1 (append list1 list2)
    mappend (Nested o1 list1) s2 = Nested o1 (snoc list1 s2)
    mappend s1 (Nested _ list2) = Nested (locationOf s1) (cons s1 list2)
    mappend s1 s2 = Nested (locationOf s1) (snoc (singleton s1) s2)
