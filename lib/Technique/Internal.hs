{-# LANGUAGE GADTs #-}

{-|
Given a Technique Procedure (surface syntax tree), transform it into an
internalized representation (abstract syntax tree) that can be subsequently
executed (that is, interpreted; evaluated).
-}
module Technique.Internal where

import Core.Data
import Core.Text
import Data.UUID.Types (UUID)

import Technique.Language
import Technique.Quantity

{-|
Environment in the type-theory sense of the word: the map(s) between names
and their bindings.
-}
data Environment = Environment
    { environmentVariables :: Map Name Promise
    , environmenFunctions :: Map Identifier Procedure
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
-- I'm incredibly unhappy with this name because "function" means something
-- in functional programming and in programming language theory and this
-- isn't it. Alternatives considered include Instance (the original name,
-- but we've reserved that to be used when instantiating a procedure at
-- runtime), Representation, Internal.
data Function = Function
    { functionSource :: Procedure
    , functionRole :: Attribute
    , functionSteps :: [Step]
    }

data Primitive = Primitive
    { primitiveName :: Identifier
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
    | Asynchronous Name Step            -- assignment (ie lambda, "implication introduction"
    | Invocation Function Step          -- function application ("implication elimination") on a [sub] Procedure
    | External Primitive Step           -- same, but calling a primative builtin.
    | Tuple (Step,Step)

                                        -- assumption axiom?
                                        -- weakening?

{-|
Take a static Procedure definition and spin it up into a "Function"
suitable for interpretation. In other words, translate between the concrete
syntax types and the abstract syntax we can feed to an evaluator.
-}
internalize :: Environment -> Procedure -> Function
internalize env procedure =
    Function
        { functionSource = procedure
        , functionSteps = foldr f [] statements
        }
  where
    block = procedureBlock procedure
    statements = blockStatements block

    f :: [Step] -> Statement -> [Step]
    f steps statement = steps <> internalizeStatement env statement


internalizeStatement :: Environment -> Statement -> [Step]
internalizeStatement context statement = case statement of
    Assignment vars expr -> Asynchronous

    Execute expr -> internalizeExpression context expr

    Declaration proc -> insertProcedure context proc

    -- the remainder are functionally no-ops
    Comment _ -> []
    Blank -> []
    Series -> []

internalizeExpression :: Environment -> Expression -> Step
internalizeExpression context steps expr = case expr of
    Application i expr ->
        -- lookup returns a function that constructs a Step
        (lookupProcedure context i) (internalizeExpression expr)
    None ->
        Known Unitus
    Text text ->
        Known (Literali text)
    Amount qty ->
        Known (Quanticle qty)
    Undefined ->
        error "?!?" -- TODO ERROR not error but "hole, stop here"
    Object (Tablet bindings) ->
        Known (Tabularum (fmap ( \(Binding label expr) -> (label,internalizeExpression  expr)) bindings))
    Variable is ->
        Tuple (fmap ? is)
    Operation op subexpr1 subexpr2 ->
      let
        f = case op of
                WaitEither  -> waitEither context
                WaitBoth    -> waitBoth context
                Combine     -> combineValues context
      in
        External f (Tuple [(internalizeExpression env subexpr1),(internalizeExpression env subexpr2)])
    Grouping subexpr ->
        internalizeExpression subexpr
    Restriction attr block ->
        applyRestriction attr (internalizeBlock block)


{-|
A given procedure call can either be to a user declared in-scope procedure
or to a primative builtin. We have Invocation and External as the two Step
constructors for these cases. This lookup function returns a function which
is the appropriate constructor, partially applied.
-}
-- TODO ERROR this will be a hugely common spot for the compiler to
-- discover an error, in this case calling an unknown procedure. We'll need
-- *much* better error handling than this.
lookupProcedure :: Environment -> Identifier -> (Step -> Step)
lookupProcedure context i =
  let
    declared = lookupKeyValue i (contextFunctions context)
    known = lookupKeyValue i builtins
  in
    case declared of
        Just proc -> Invocation i
        Nothing -> case known of
            Just p -> External p
            Nothing -> error (fromRope ("call to unknown procedure '" <> unIdentifier i <> "'"))

insertProcedure :: Procedure -> _
insertProcedure proc =
    undefined

waitEither :: Primitive
waitEither = Primitive
    { primitiveName = Identifier "wait_either"
    , primitiveAction = \step -> case step of
        Tuple (step1,step2) -> undefined
        _ -> undefined
    }

waitBoth :: Primitive
waitBoth = Primitive
    { primitiveName = Identifier "wait_both"
    }

combineValues :: Primitive
combineValues context = Primitive
    { primitiveName = Identifier "combine_values"
    }

applyRestriction :: Attribute -> Block -> Function
applyRestriction = undefined
