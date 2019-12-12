{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Given a Technique Procedure (concrete syntax tree), translate it into an
internalized representation (abstract syntax tree) that can be subsequently
executed (that is, interpreted; evaluated).
-}
module Technique.Translate where

import Control.Monad (when, foldM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Control.Monad.Trans.Except (Except(), runExcept)
import Core.Data
import Core.Text
import Data.DList (empty, toList, fromList)
import Data.Foldable (traverse_)

import Technique.Builtins
import Technique.Failure
import Technique.Internal
import Technique.Language


{-|
Environment in the type-theory sense of the word: the map(s) between names
and their bindings.
-}
-- TODO perhaps the role should be Maybe Attribute? This will likely need
-- work as there are three states: 1) as yet unspecified, 2) specified, and
-- 3) explicitly reset to any. Are (1) and (3) the same?
data Environment = Environment
    { environmentVariables :: Map Identifier Name
    , environmentFunctions :: Map Identifier Function
    , environmentRole :: Attribute
    , environmentAccumulated :: Step
    }
    deriving (Eq,Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment
    { environmentVariables = emptyMap
    , environmentFunctions = emptyMap
    , environmentRole = Inherited
    , environmentAccumulated = NoOp
    }

newtype Translate a = Translate (StateT Environment (Except CompilerFailure) a)
    deriving (Functor, Applicative, Monad, MonadState Environment, MonadError CompilerFailure)

{-|
Take a translator action and an environment and spin it up into a Step or
nest of Steps ("Subroutine") suitable for interpretation. In other words,
translate between the concrete syntax types and the abstract syntax we can
feed to an evaluator.
-}
-- we use runStateT rather than evalStateT as we did previously so we can
-- access the final state in test cases.
runTranslate :: Environment -> Translate a -> Either CompilerFailure (a,Environment)
runTranslate env (Translate action) = runExcept (runStateT action env)
{-# INLINE runTranslate #-}


translateTechnique :: Technique -> Translate [Function]
translateTechnique technique = do
    -- Stage 1: conduct translation
    funcs1 <- traverse translateProcedure (techniqueBody technique)

    -- Stage 2: resolve functions
    funcs2 <- traverse resolver funcs1
    return funcs2
  where
    resolver :: Function -> Translate Function
    resolver func = case func of
        Subroutine proc step -> do
            step' <- resolveFunctions step
            return (Subroutine proc step')

        _ -> error ("Illegal state: How did you get a top level " ++ (show func) ++ "?")

translateProcedure :: Procedure -> Translate Function
translateProcedure procedure =
  let
    block = procedureBlock procedure
  in do
    env <- get

    let subenv = env    -- placeholder in case we need to refine
    let result = runTranslate subenv (translateBlock block)

    case result of
        Left e -> failBecause e
        Right (step,_) -> do
            let func = Subroutine procedure step
            registerProcedure func
            return func

{-|
Blocks are scoping mechanisms, so accumulated environment is discarded once
we finish resolving names within it.
-}
translateBlock :: Block -> Translate Step
translateBlock (Block statements) = do
    traverse_ translateStatement statements

    env' <- get
    let step = environmentAccumulated env'
    return step

translateStatement :: Statement -> Translate ()
translateStatement statement = case statement of
    Assignment vars expr -> do
        names <- traverse insertVariable vars
        step <- translateExpression expr

        let step' = Asynchronous names step

        appendStep step'

    Execute expr -> do
        step <- translateExpression expr
        appendStep step


    Declaration proc -> do
        _ <- translateProcedure proc
        return ()

    -- the remainder are functionally no-ops
    Comment _ -> return ()
    Blank -> return ()
    Series -> return ()

{-|
Note that this does NOT add the steps to the Environment.
-}
translateExpression :: Expression -> Translate Step
translateExpression expr = do
    env <- get
    let attr = environmentRole env

    case expr of
        Application i subexpr -> do
            let func = Unresolved i
            step <- translateExpression subexpr
            return (Invocation attr func step)

        None ->
            return (Known Unitus)

        Text text ->
            return (Known (Literali text))

        Amount qty ->
            return (Known (Quanticle qty))

        Undefined ->
            failBecause EncounteredUndefined

        Object (Tablet bindings) -> do
            pairs <- foldM f [] bindings
            return (Bench pairs)
          where
            f :: [(Label,Step)] -> Binding -> Translate [(Label,Step)]
            f acc (Binding label subexpr) = do
                step <- translateExpression subexpr
                return (acc <> [(label,step)])

        Variable is -> do
            steps <- traverse g is
            case steps of
                [] -> return (Nested empty)
                [step] -> return step
                _ -> return (Tuple steps)
          where
            g :: Identifier -> Translate Step
            g i = do
                name <- lookupVariable i
                return (Depends name)

        Operation op subexpr1 subexpr2 ->
          let
            prim = case op of
                    WaitEither  -> builtinProcedureWaitEither
                    WaitBoth    -> builtinProcedureWaitBoth
                    Combine     -> builtinProcedureCombineValues
          in do
            step1 <- translateExpression subexpr1
            step2 <- translateExpression subexpr2
            let tuple = Tuple [step1,step2]
            return (Invocation attr prim tuple)

        Grouping subexpr ->
            translateExpression subexpr

        Restriction subattr block ->
            applyRestriction subattr block


{-|
A given procedure call can either be to a user declared in-scope procedure
or to a primative builtin. We have Invocation as the Step constructors for
these cases.
-}
registerProcedure :: Function -> Translate ()
registerProcedure func = do
    env <- get

    let i = functionName func
    let known = environmentFunctions env
    let defined = containsKey i known

    when defined $ do
        failBecause (ProcedureAlreadyDeclared i)

    let known' = insertKeyValue i func known
    let env' = env { environmentFunctions = known' }

    put env'

-- the overloading of throw between MonadError / ExceptT and the GHC
-- exceptions mechansism is unfortunate. We're not throwing an exception,
-- end it's definitely not pure `error`. Wrap it for clarity.
failBecause :: CompilerFailure -> Translate a
failBecause e = throwError e

lookupVariable :: Identifier -> Translate Name
lookupVariable i = do
    env <- get
    let known = lookupKeyValue i (environmentVariables env)

    case known of
        Just name -> return name
        Nothing -> failBecause (UseOfUnknownIdentifier i)

{-|
Identifiers are valid names but Names are unique, so that we can put
them into the environment map. This is where we check for reuse of an
already declared name (TODO) and given the local use of the identifier a
scope-local (or globally?) unique name.
-}
insertVariable :: Identifier -> Translate Name
insertVariable i = do
    env <- get
    let known = environmentVariables env
    when (containsKey i known) $ do
        failBecause (VariableAlreadyInUse i)

    let n = Name (singletonRope '!' <> unIdentifier i) -- TODO

    let known' = insertKeyValue i n known
    let env' = env { environmentVariables = known' }
    put env'
    return n

{-|
Accumulate a Step
-}
appendStep :: Step -> Translate ()
appendStep step = do
    env <- get
    let steps = environmentAccumulated env

    -- see the Monoid instance for Step for the clever here
    let steps' = mappend steps step

    let env' = env { environmentAccumulated = steps' }
    put env'

{-|
This begins a new (more refined) scope and does *not* add its declarations
or variables to the current environment.
-}
applyRestriction :: Attribute -> Block -> Translate Step
applyRestriction attr block = do
    env <- get

    let subenv = env
            { environmentRole = attr
            }

    let result = runTranslate subenv (translateBlock block)

    case result of
        Left e -> failBecause e
        Right (steps,_) -> return steps


-----------------------------------------------------------------------------

{-|
The second stage of translation phase: iterate through the Steps and where
a function call is made, look up to see if we actually know what it is.
-}
resolveFunctions :: Step -> Translate Step
resolveFunctions step = case step of
    Invocation attr func substep -> do
        func' <- lookupFunction func
        substep' <- resolveFunctions substep
        return (Invocation attr func' substep')

    Tuple substeps -> do
        substeps' <- traverse resolveFunctions substeps
        return (Tuple substeps')

    Asynchronous names substep -> do
        substep' <- resolveFunctions substep
        return (Asynchronous names substep')

    Nested sublist -> do
        let actual = toList sublist
        actual' <- traverse resolveFunctions actual
        let sublist' = fromList actual'
        return (Nested sublist')

    Bench pairs -> do
        pairs' <- traverse f pairs
        return (Bench pairs')
      where
        f :: (Label,Step) -> Translate (Label,Step)
        f (label,substep) = do
            substep' <- resolveFunctions substep
            return (label, substep')

    Known _ -> return step
    Depends _ -> return step
    NoOp -> return step


lookupFunction :: Function -> Translate Function
lookupFunction func = do
    env <- get

    let i = functionName func
        known = environmentFunctions env
        result = lookupKeyValue i known

    case result of
        Nothing -> failBecause (CallToUnknownProcedure i)
        Just actual -> return actual
