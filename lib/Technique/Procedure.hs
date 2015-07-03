--
-- Procedures
--
-- Copyright © 2012-2015 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is made available
-- to you by its authors as open source software: you can redistribute it
-- and/or modify it under the terms of the GNU General Public License version
-- 2 ("GPL") as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
--
-- You should have received a copy of the GPL along with this program. If not,
-- see http://www.gnu.org/licenses/. The authors of this program may be
-- contacted through http://research.operationaldynamics.com/
--

module Technique.Procedure where

import Control.Monad
import Control.Shell

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
    | Process (Shell a)


