{-# LANGUAGE GADTs #-}

{-|
Given a Technique Procedure (concrete syntax tree), translate it into an
internalized representation (abstract syntax tree) that can be subsequently
executed (that is, interpreted; evaluated).
-}
module Technique.Translate where

import Core.Data
import Core.Text
import Data.Foldable (foldl')
import Data.UUID.Types (UUID)

import Technique.Builtins
import Technique.Internal
import Technique.Language
import Technique.Quantity

{-|
Take a static Procedure definition and spin it up into a "Subroutine"
suitable for interpretation. In other words, translate between the concrete
syntax types and the abstract syntax we can feed to an evaluator.
-}
translate :: Environment -> Procedure -> Subroutine
translate env procedure =
    Subroutine
        { subroutineSource = procedure
        , subroutineSteps = translateBlock env (procedureBlock procedure)
        }

-- blocks are scoping mechanisms, so accumulated environment is discarded
-- once we finish resolving names within it.
translateBlock :: Environment -> Block -> [Step]
translateBlock env0 (Block statements) = snd (foldl' f (env0,[]) statements)
  where
    f :: (Environment,[Step]) -> Statement -> (Environment,[Step])
    f (env,steps) statement =
      let
        (env',steps') = translateStatement env statement
      in
        (env',steps <> steps')

translateStatement :: Environment -> Statement -> (Environment,[Step])
translateStatement env statement = case statement of
    Assignment vars expr ->
      let
        env',names = fmap createVariable vars
        step = Asynchronous names (translateExpression expr)
      in
        undefined

    Execute expr ->
        translateExpression env expr

    Declaration proc ->
        insertProcedure env proc

    -- the remainder are functionally no-ops
    Comment _ -> []
    Blank -> []
    Series -> []

translateExpression :: Environment -> Expression -> [Step]
translateExpression env expr = case expr of
    Application i expr ->
        -- lookup returns a function that constructs a Step
        (lookupProcedure env i) (translateExpression env expr)
    None ->
        [Known Unitus]
    Text text ->
        [Known (Literali text)]
    Amount qty ->
        [Known (Quanticle qty)]
    Undefined ->
        error "?!?" -- TODO ERROR not error but "hole, stop here"
    Object (Tablet bindings) ->
        Known (Tabularum (fmap ( \(Binding label expr) -> (label,translateExpression env expr)) bindings)):[]
    Variable is ->
        Tuple (fmap Depends is)
    Operation op subexpr1 subexpr2 ->
      let
        f = case op of
                WaitEither  -> builtinProcedureWaitEither
                WaitBoth    -> builtinProcedureWaitBoth
                Combine     -> builtinProcedureCombineValues
      in
        External f (Tuple [(translateExpression env subexpr1),(translateExpression env subexpr2)])
    Grouping subexpr ->
        translateExpression env subexpr
    Restriction attr block ->
        applyRestriction attr (translateBlock env block)


{-|
A given procedure call can either be to a user declared in-scope procedure
or to a primative builtin. We have Invocation and External as the two Step
constructors for these cases. This lookup function returns a function which
is the appropriate constructor, partially applied.
-}
-- TODO ERROR this will be a hugely common spot for the compiler to
-- discover an error, in this case calling an unknown procedure. We'll need
-- *much* better error handling than this.
lookupProcedure :: Environment -> Identifier -> ([Step] -> Step)
lookupProcedure env i =
  let
    declared = lookupKeyValue i (environmentFunctions env)
    known = lookupKeyValue i builtins
  in
    case declared of
        Just proc -> Invocation i
        Nothing -> case known of
            Just p -> External p
            Nothing -> error (fromRope ("call to unknown procedure '" <> unIdentifier i <> "'"))

insertProcedure :: Procedure -> ()
insertProcedure proc =
    undefined

{-|
Identifiers are valid names but Names are unique, so that we can put
them into the environment map. This is where we check for reuse of an
already declared name (TODO) and given the local use of the identifier a
locally unique name.
-}
createVariable :: Environment -> Identifier -> (Environment,Name)
createVariable env i =
  let
    known = environmentVariables env
    name = Name (singletonRope '!' <> unIdentifier i) -- FIXME
    known' = insertKeyValue i name known
    env' = env { environmentVariables = known' }
  in
    (env',name)

applyRestriction :: Attribute -> Block -> () -- ???
applyRestriction = undefined
