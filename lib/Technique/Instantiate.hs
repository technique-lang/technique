{-# LANGUAGE GADTs #-}

{-|
Given a Technique Procedure, transform it into an Instance that can be executed.
-}
module Technique.Instantiate where

import Core.Data
import Core.Text
import Data.UUID.Types (UUID)

import Technique.Language
import Technique.Quantity


{-|
In order to instantiate a Procedure into something that can in turn be
interpreted and thus run, we need to supply a Context: an identifier for
the event (collection of procedure calls) it is a part of, and the path
history we took to get here.
-}
data Context = Context
    { contextEvent :: UUID
    , contextPath :: Rope -- or a  list or a fingertree or...
    , contextVariables :: Map Name Promise
    , contextFunctions :: Map Identifier Procedure
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

data Instance = Instance
    { instanceContext :: Context
    , instanceSource :: Procedure
    , instanceRole :: Attribute
    , instanceSteps :: [Step]
    }

data Primitive = Primitive
    { primitiveContext :: Context   -- do we need this?
    , primitiveName :: Identifier
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
    | Invocation Instance Step          -- function application ("implication elimination") on a [sub] Procedure
    | External Primitive Step           -- same, but calling a primative builtin.
    | Tuple (Step,Step)

                                        -- assumption axiom?
                                        -- weakening?

{-|
Take a static Procedure definition and spin it up into an Instance suitable
for interpretation. In other words, translate between the surface syntax types
and the abstract syntax we can feed to an evaluator.
-}
instantiate :: Context -> Procedure -> Instance
instantiate context procedure =
    Instance
        { instanceContext = context
        , instanceSource = procedure
        , instanceSteps = foldr f [] statements
        }
  where
    block = procedureBlock procedure
    statements = blockStatements block

    f :: [Step] -> Statement -> [Step]
    f steps statement = steps <> instantiateStatement context statement


instantiateStatement :: Context -> Statement -> [Step]
instantiateStatement context statement = case statement of
    Assignment vars expr -> Asynchronous

    Execute expr -> instantiateExpression context expr

    Declaration proc -> insertProcedure context proc

    -- the remainder are functionally no-ops
    Comment _ -> []
    Blank -> []
    Series -> []

instantiateExpression :: Context -> Expression -> Step
instantiateExpression context steps expr = case expr of
    Application i expr ->
        -- lookup returns a function that constructs a Step
        (lookupProcedure context i) (instantiateExpression expr)
    None ->
        Known Unitus
    Text text ->
        Known (Literali text)
    Amount qty ->
        Known (Quanticle qty)
    Undefined ->
        error "?!?" -- TODO ERROR not error but "hole, stop here"
    Object (Tablet bindings) ->
        Known (Tabularum (fmap ( \(Binding label expr) -> (label,instantiateExpression  expr)) bindings))
    Variable is ->
        Tuple (fmap ? is)
    Operation op subexpr1 subexpr2 ->
      let
        f = case op of
                WaitEither  -> waitEither context
                WaitBoth    -> waitBoth context
                Combine     -> combineValues context
      in
        External f (Tuple [(instantiateExpression env subexpr1),(instantiateExpression env subexpr2)])
    Grouping subexpr ->
        instantiateExpression subexpr
    Restriction attr block ->
        applyRestriction attr (instantiateBlock block)


{-|
A given procedure call can either be to a user declared in-scope procedure
or to a primative builtin. We have Invocation and External as the two Step
constructors for these cases. This lookup function returns a function which
is the appropriate constructor, partially applied.
-}
-- TODO ERROR this will be a hugely common spot for the compiler to
-- discover an error, in this case calling an unknown procedure. We'll need
-- *much* better error handling than this.
lookupProcedure :: Context -> Identifier -> (Step -> Step)
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

insertProcedure :: Context -> Procedure -> _
insertProcedure context proc =
    undefined

waitEither :: Context -> Primitive
waitEither context = Primitive
    { primitiveContext = context
    , primitiveName = Identifier "wait_either"
    , primitiveAction = \step -> case step of
        Tuple (step1,step2) -> undefined
        _ -> undefined
    }

waitBoth :: Context -> Primitive
waitBoth context = Primitive
    { primitiveContext = context
    , primitiveName = Identifier "wait_both"
    }

combineValues :: Context -> Primitive
combineValues context = Primitive
    { primitiveContext = context
    , primitiveName = Identifier "combine_values"
    }

applyRestriction :: Attribute -> Block -> Context
applyRestriction = undefined
