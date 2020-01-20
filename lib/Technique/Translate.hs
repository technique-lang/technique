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
        Left e -> throwError e
        Right (step,_) -> do
            let func = Subroutine procedure step
            registerProcedure (locationOf step) func
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

        Operation offset op subexpr1 subexpr2 ->
          let
            prim = case op of
                    WaitEither  -> builtinProcedureWaitEither
                    WaitBoth    -> builtinProcedureWaitBoth
                    Combine     -> builtinProcedureCombineValues
          in do
            step1 <- translateExpression subexpr1
            step2 <- translateExpression subexpr2
            let tuple = Tuple offset [step1,step2]
            return (Invocation offset attr prim tuple)

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
registerProcedure offset func = do
    env <- get

    let i = functionName func
    let known = environmentFunctions env
    let defined = containsKey i known

    when defined $ do
        failBecause offset (ProcedureAlreadyDeclared i)

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
lookupVariable offset i = do
    env <- get
    let known = lookupKeyValue i (environmentVariables env)

    case known of
        Just name -> return name
        Nothing -> failBecause offset (UseOfUnknownIdentifier i)

{-|
Identifiers are valid names but Names are unique, so that we can put
them into the environment map. This is where we check for reuse of an
already declared name (TODO) and given the local use of the identifier a
scope-local (or globally?) unique name.
-}
insertVariable :: Offset -> Identifier -> Translate Name
insertVariable offset i = do
    env <- get
    let known = environmentVariables env

    when (containsKey i known) $ do
        failBecause offset (VariableAlreadyInUse i)

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
    Invocation offset attr func substep -> do
        func' <- lookupFunction offset func
        substep' <- resolveFunctions substep
        return (Invocation offset attr func' substep')

    Tuple offset substeps -> do
        substeps' <- traverse resolveFunctions substeps
        return (Tuple offset substeps')

    Asynchronous offset names substep -> do
        substep' <- resolveFunctions substep
        return (Asynchronous offset names substep')

    Nested offset sublist -> do
        let actual = toList sublist
        actual' <- traverse resolveFunctions actual
        let sublist' = fromList actual'
        return (Nested offset sublist')

    Bench offset pairs -> do
        pairs' <- traverse f pairs
        return (Bench offset pairs')
      where
        f :: (Label,Step) -> Translate (Label,Step)
        f (label,substep) = do
            substep' <- resolveFunctions substep
            return (label, substep')

    Known _ _ -> return step
    Depends _ _ -> return step
    NoOp -> return step

lookupFunction :: Offset -> Function -> Translate Function
lookupFunction offset func = do
    env <- get

    let i = functionName func
        known = environmentFunctions env
        result = lookupKeyValue i known

    case result of
        Nothing -> failBecause offset (CallToUnknownProcedure i)
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
    let offset = locationOf thing
    let source' = source { sourceOffset = offset }
    let env' = env { environmentSource = source' }
    put env'

