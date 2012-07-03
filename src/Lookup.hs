--
-- Procedures
--
-- Copyright © 2011-2012 Operational Dynamics Consulting, Pty Ltd
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

{-# LANGUAGE OverloadedStrings #-}

module Lookup (lookupResource, storeResource) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Database.Redis
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (MonadCatchIO, bracket)

--
-- Store and Access keys from Redis database. This treats unset and empty as
-- the same.
--

fromReply :: (Either Reply (Maybe S.ByteString)) -> S.ByteString
fromReply x = 
    either first second x
  where
    first :: Reply -> S.ByteString
    first (Error s) = s
    first other     = "Kaboom!\n" 

    second :: (Maybe S.ByteString) -> S.ByteString
    second = fromMaybe ""


queryResource :: Int -> Redis S.ByteString
queryResource x = do
    k <- get key
    return $ fromReply k
  where
    key = S.append "resource:" $ S.pack $ show x


--
-- Establish connection to redis database and conduct query. This is more or
-- less lifting from the IO monad to Redis monad, but you connection to an
-- external entity (ie, socket connection to database) has to happen in IO.
--

settings =
    defaultConnectInfo {
        connectPort = UnixSocket "./redis.sock"
    }
    
lookupResource :: Int -> IO S.ByteString
lookupResource d = bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ queryResource d)


writeResource :: Int -> Int -> Redis ()
writeResource d t = do
    set key value
    return ()
  where
    key   = S.append "resource:" $ S.pack $ show d
    value = S.pack $ show t



storeResource :: Int -> Int -> IO ()
storeResource d t = bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ writeResource d t)

