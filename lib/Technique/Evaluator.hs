{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Given an instantiated Technique Procedure, evalutate it at runtime.
-}
-- At present this is a proof of concept. It might benefit from being
-- converted to a typeclass in the tagless final style.
module Technique.Evaluator where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Core.Data
import Core.Text
import Data.UUID.Types (UUID)

import Technique.Instantiate
import Technique.Language
import Technique.Quantity

-- TODO this needs to evolve to IVars or equivalent
data Environment = Environment (Map Identifier Value)

{-
data Expression b where
    Binding :: Variable b -> Expression a -> Expression b
    Comment :: Rope -> Expression ()
    Declaration :: (a -> b) -> Expression (a -> b)
    Application :: Expression (a -> b) -> Expression a -> Expression b 
    Attribute :: Role -> Expression a -> Expression a
-}

-- Does this need to upgrade to a MonadEvaluate mtl style class in order to
-- support different interpeters / backends? This seems so cumbersome
-- compared to the elegent tagless final method.

newtype Evaluate a = Evaluate (ReaderT Environment IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Environment)

unEvaluate :: Evaluate a -> ReaderT Environment IO a
unEvaluate (Evaluate r) = r

{-|
The heart of the evaluation loop. Translate from the abstract syntax tree 
into a monadic sequence which results in a Result.
-}
evaluate :: Environment -> Step -> Evaluate Value
evaluate env step = case step of
    Known value -> do
        return value

    Depends name -> do
        blockUntilValue name

    Asynchronous name step -> do
        assignName name step

    Invocation inst step -> do
        functionApplication inst step

functionApplication :: Instance -> Step -> Evaluate Value --  IO Promise ?
functionApplication = undefined

blockUntilValue :: Name -> Evaluate Value
blockUntilValue = undefined

{-|
Take a step and lauch it asynchronously, binding its result to a name.
Returns a promise of a value that can be in evaluated (block on) when
needed.
-}
assignName :: Name -> Step -> Evaluate ()
assignName = do
    -- dunno
    return (Parametri []) -- fixme not empty list
