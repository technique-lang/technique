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
import Data.DList (empty)
import Data.Foldable (traverse_)

import Technique.Builtins
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
    , environmentFunctions :: Map Identifier Subroutine
    , environmentRole :: Attribute
    , environmentAccumulated :: Step
    }
    deriving (Eq,Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment
    { environmentVariables = emptyMap
    , environmentFunctions = emptyMap
    , environmentRole = Unspecified
    , environmentAccumulated = Sequence (empty)
    }

newtype Translate a = Translate (StateT Environment (Except CompilerFailure) a)
    deriving (Functor, Applicative, Monad, MonadState Environment, MonadError CompilerFailure)

-- we use runStateT rather than evalStateT as we did previously so we can
-- access the final state in test cases.
runTranslate :: Environment -> Translate a -> Either CompilerFailure (a,Environment)
runTranslate env (Translate action) = runExcept (runStateT action env)
{-# INLINE runTranslate #-}

{-|
Take a static Procedure definition and spin it up into a "Subroutine"
suitable for interpretation. In other words, translate between the concrete
syntax types and the abstract syntax we can feed to an evaluator.
-}
translate :: Environment -> Procedure -> Either CompilerFailure Subroutine
translate env procedure =
  let
    result = runTranslate env (translateProcedure procedure)
  in
    case result of
        Left e -> Left e
        Right (subroutine,_) -> Right subroutine

translateProcedure :: Procedure -> Translate Subroutine
translateProcedure procedure =
  let
    block = procedureBlock procedure
  in do
    step <- translateBlock block
    return
        (Subroutine
            { subroutineSource = procedure
            , subroutineSteps = step
            }
        )

{-|
Blocks are scoping mechanisms, so accumulated environment is discarded once
we finish resolving names within it.
-}
-- traverse_ is "new", just mapM_ at Applicative? Lets see how we feel
-- about that.
translateBlock :: Block -> Translate Step
translateBlock (Block statements) = do
    traverse_ translateStatement statements
    env' <- get
    return (environmentAccumulated env')

translateStatement :: Statement -> Translate ()
translateStatement statement = case statement of
    Assignment vars expr -> do
        names <- traverse createVariable vars
        step <- translateExpression expr

        let step' = Asynchronous names step

        appendStep step'

    Execute expr -> do
        step <- translateExpression expr
        appendStep step


    Declaration proc -> do
        insertProcedure proc

    -- the remainder are functionally no-ops
    Comment _ -> return ()
    Blank -> return ()
    Series -> return ()

-- TODO HERE this does NOT add the steps to the Environment

translateExpression :: Expression -> Translate Step
translateExpression expr = do
    env <- get
    let attr = environmentRole env

    case expr of
        Application i subexpr -> do
            -- lookup returns a function that constructs a Step
            func <- lookupProcedure i
            step <- translateExpression subexpr
            return (func step)

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
            return (Tuple steps)
          where
            g :: Identifier -> Translate Step
            g i =
              let
                known = environmentVariables env
                result = lookupKeyValue i known
              in case result of
                Nothing -> failBecause (UseOfUnknownIdentifier i)
                Just name -> return (Depends name)

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
            return (External attr prim tuple)

        Grouping subexpr ->
            translateExpression subexpr

        Restriction subattr block ->
            applyRestriction subattr block


{-|
A given procedure call can either be to a user declared in-scope procedure
or to a primative builtin. We have Invocation and External as the two Step
constructors for these cases. This lookup function returns a function which
is the appropriate constructor, partially applied.
-}
-- TODO ERROR this will be a hugely common spot for the compiler to
-- discover an error, in this case calling an unknown procedure. We'll need
-- *much* better error handling than this.
lookupProcedure :: Identifier -> Translate (Step -> Step)
lookupProcedure i = do
    env <- get
    let declared = lookupKeyValue i (environmentFunctions env)
    let known = lookupKeyValue i builtins
    let attr = environmentRole env

    case declared of
        Just subroutine -> return (Invocation attr subroutine)
        Nothing -> case known of
            Just primitive -> return (External attr primitive)
            Nothing -> failBecause (CallToUnknownProcedure i)

insertProcedure :: Procedure -> Translate ()
insertProcedure proc = do
    env <- get
    let known = environmentFunctions env
        i = procedureName proc

    when (containsKey i known) $ do
        failBecause (ProcedureAlreadyDeclared i)

    subroutine <- translateProcedure proc

    let known' = insertKeyValue i subroutine known
        env' = env { environmentFunctions = known' }

    put env'

-- the overloading of throw between MonadError / ExceptT and the GHC
-- exceptions mechansism is unfortunate. We're not throwing an exception,
-- end it's definitely not pure `error`. Wrap it for clarity.
failBecause :: CompilerFailure -> Translate a
failBecause e = throwError e

{-|
Identifiers are valid names but Names are unique, so that we can put
them into the environment map. This is where we check for reuse of an
already declared name (TODO) and given the local use of the identifier a
scope-local (or globally?) unique name.
-}
createVariable :: Identifier -> Translate Name
createVariable i = do
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
    let steps' = steps <> step

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
