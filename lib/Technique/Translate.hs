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
import Data.DList (toList, fromList)
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

    -- for reporting compiler errors
    , environmentSource :: Source

    -- the accumulator for the fold that the Translate monad represents
    , environmentAccumulated :: Step
    }
    deriving (Eq,Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment
    { environmentVariables = emptyMap
    , environmentFunctions = emptyMap
    , environmentRole = Inherit
    , environmentSource = emptySource
    , environmentAccumulated = NoOp
    }

newtype Translate a = Translate (StateT Environment (Except CompilationError) a)
    deriving (Functor, Applicative, Monad, MonadState Environment, MonadError CompilationError)

{-|
Take a translator action and an environment and spin it up into a Step or
nest of Steps ("Subroutine") suitable for interpretation. In other words,
translate between the concrete syntax types and the abstract syntax we can
feed to an evaluator.
-}
-- we use runStateT rather than evalStateT as we did previously so we can
-- access the final state in test cases.
runTranslate :: Environment -> Translate a -> Either CompilationError (a,Environment)
runTranslate env (Translate action) = runExcept (runStateT action env)
{-# INLINE runTranslate #-}


translateTechnique :: Technique -> Translate Executable
translateTechnique technique = do
    -- Stage 1: conduct translation
    funcs1 <- traverse translateProcedure (techniqueBody technique)

    -- Stage 2: resolve functions
    funcs2 <- traverse resolver funcs1
    return (Executable funcs2)
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
    is = procedureParams procedure
    o = procedureOffset procedure
    block = procedureBlock procedure
  in do
    env <- get

-- calling runTranslate here *is* the act of refining, but there's no way
-- we're going to remember that so make it explicit. Gives us the
-- opportunity to modify the environment before descending if necessary.

    let subenv = env
    let result = runTranslate subenv $ do
            traverse_ (insertVariable o) is
            translateBlock block

    case result of
        Left e -> throwError e
        Right (step,_) -> do
            let func = Subroutine procedure step
            registerProcedure (locationOf procedure) func
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
translateStatement statement = do

    case statement of
        Assignment o vars expr -> do
            -- FIXME this offset will be incorrect if > 1 variable.
            names <- traverse (insertVariable o) vars
            step <- translateExpression expr

            let step' = Asynchronous o names step
            appendStep step'

        Execute _ expr -> do
            step <- translateExpression expr
            appendStep step

        Declaration _ proc -> do
            _ <- translateProcedure proc
            return ()

        -- the remainder are functionally no-ops
        Comment _ _ -> return ()
        Blank _ -> return ()
        Series _ -> return ()

{-|
Note that this does NOT add the steps to the Environment.
-}
translateExpression :: Expression -> Translate Step
translateExpression expr = do
    env <- get
    let attr = environmentRole env

    case expr of
        Application o i subexpr -> do
            let func = Unresolved i
            step <- translateExpression subexpr
            return (Invocation o attr func step)

        None o ->
            return (Known o Unitus)

        Text o text ->
            return (Known o (Literali text))

        Amount o qty ->
            return (Known o (Quanticle qty))

        Undefined o -> do
            failBecause o EncounteredUndefined

        Object o (Tablet bindings) -> do
            pairs <- foldM f [] bindings
            return (Bench o pairs)
          where
            f :: [(Label,Step)] -> Binding -> Translate [(Label,Step)]
            f acc (Binding label subexpr) = do
                step <- translateExpression subexpr
                return (acc <> [(label,step)])

        Variable o is -> do
            steps <- traverse g is
            case steps of
                [] -> return NoOp
                [step] -> return step
                _ -> return (Tuple o steps)
          where
            g :: Identifier -> Translate Step
            g i = do
                name <- lookupVariable o i
                return (Depends o name)

        Operation o oper subexpr1 subexpr2 ->
          let
            prim = case oper of
                    WaitEither  -> builtinProcedureWaitEither
                    WaitBoth    -> builtinProcedureWaitBoth
                    Combine     -> builtinProcedureCombineValues
          in do
            step1 <- translateExpression subexpr1
            step2 <- translateExpression subexpr2
            let tuple = Tuple o [step1,step2]
            return (Invocation o attr prim tuple)

        Grouping _ subexpr ->
            translateExpression subexpr

        Restriction _ subattr block ->
            applyRestriction subattr block


{-|
A given procedure call can either be to a user declared in-scope procedure
or to a primative builtin. We have Invocation as the Step constructors for
these cases.
-}
registerProcedure :: Offset -> Function -> Translate ()
registerProcedure o func = do
    env <- get

    let i = functionName func
    let known = environmentFunctions env
    let defined = containsKey i known

    when defined $ do
        failBecause o (ProcedureAlreadyDeclared i)

    let known' = insertKeyValue i func known
    let env' = env { environmentFunctions = known' }

    put env'

-- the overloading of throw between MonadError / ExceptT and the GHC
-- exceptions mechansism is unfortunate. We're not throwing an exception,
-- end it's definitely not pure `error`. Wrap it for clarity.
failBecause :: Offset -> FailureReason -> Translate a
failBecause o reason = do
    env <- get
    let source = environmentSource env
    let source' = source { sourceOffset = o }

    let failure = CompilationError source' reason
    throwError failure


lookupVariable :: Offset -> Identifier -> Translate Name
lookupVariable o i = do
    env <- get
    let known = lookupKeyValue i (environmentVariables env)

    case known of
        Just name -> return name
        Nothing -> failBecause o (UseOfUnknownIdentifier i)

{-|
Identifiers are valid names but Names are unique, so that we can put
them into the environment map. This is where we check for reuse of an
already declared name (TODO) and given the local use of the identifier a
scope-local (or globally?) unique name.
-}
insertVariable :: Offset -> Identifier -> Translate Name
insertVariable o i = do
    env <- get
    let known = environmentVariables env

    when (containsKey i known) $ do
        failBecause o (VariableAlreadyInUse i)

    let n = Name (singletonRope '!' <> unIdentifier i) -- TODO

    let known' = insertKeyValue i n known
    let env' = env { environmentVariables = known' }
    put env'
    return n

{-|
Accumulate a Step.
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
        Left e -> throwError e
        Right (steps,_) -> return steps


-----------------------------------------------------------------------------

{-|
The second stage of translation phase: iterate through the Steps and where
a function call is made, look up to see if we actually know what it is.
-}
resolveFunctions :: Step -> Translate Step
resolveFunctions step = case step of
    Invocation o attr func substep -> do
        func' <- lookupFunction o func
        substep' <- resolveFunctions substep
        return (Invocation o attr func' substep')

    Tuple o substeps -> do
        substeps' <- traverse resolveFunctions substeps
        return (Tuple o substeps')

    Asynchronous o names substep -> do
        substep' <- resolveFunctions substep
        return (Asynchronous o names substep')

    Nested o sublist -> do
        let actual = toList sublist
        actual' <- traverse resolveFunctions actual
        let sublist' = fromList actual'
        return (Nested o sublist')

    Bench o pairs -> do
        pairs' <- traverse f pairs
        return (Bench o pairs')
      where
        f :: (Label,Step) -> Translate (Label,Step)
        f (label,substep) = do
            substep' <- resolveFunctions substep
            return (label, substep')

    Known _ _ -> return step
    Depends _ _ -> return step
    NoOp -> return step

lookupFunction :: Offset -> Function -> Translate Function
lookupFunction o func = do
    env <- get

    let i = functionName func
        known = environmentFunctions env
        result = lookupKeyValue i known

    case result of
        Nothing -> failBecause o (CallToUnknownProcedure i)
        Just actual -> return actual

{-|
Update the environment's idea of where in the source we are, so that if we
need to generate an error message we can offer one with position
information.
-}
setLocationFrom :: (Render a, Located a) => a -> Translate ()
setLocationFrom thing = do
    env <- get
    let source = environmentSource env
    let o = locationOf thing
    let source' = source { sourceOffset = o }
    let env' = env { environmentSource = source' }
    put env'

