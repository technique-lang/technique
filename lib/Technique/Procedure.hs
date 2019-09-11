module Technique.Procedure where

import Control.Monad

--
-- A procedure is its dependencies; it's current action IS A
-- DEPENDENCY
--

{-
newtype Procedure a = Procedure a
    deriving (Functor, Monad)
-}

data Procedure a = Procedure {
    unwrap :: a
}

instance Functor Procedure where
    fmap f (Procedure x) = Procedure (f x)

instance Applicative Procedure where
    pure  = return
    (<*>) = ap

instance Monad Procedure where
    return x            = Procedure x
    (Procedure x) >>= f = f x


runProcedure :: Procedure a -> IO a
runProcedure = return . unwrap

data Action a
    = Empty


