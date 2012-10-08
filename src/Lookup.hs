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

module Lookup (lookupResource, storeResource, flushDatastore) where

import qualified Data.ByteString.Char8 as S
import Database.Redis
import Control.Monad.CatchIO (bracket)

--
-- Store and Access keys from Redis database. If the key is unset returns
-- Nothing.
--

fromReply :: (Either Reply (Maybe S.ByteString)) -> Maybe S.ByteString
fromReply x =
    case x of
        Right sm'       -> sm'
        Left (Error s') -> Just s'
        Left _          -> Just "Kaboom!\n"     -- Is this even possible?


queryResource :: S.ByteString -> Redis (Maybe S.ByteString)
queryResource x' = do
    k <- get key'
    return $ fromReply k
  where
    key' = S.append "resource:" x'


--
-- Establish connection to redis database and conduct query. This is more or
-- less lifting from the IO monad to Redis monad, but you connection to an
-- external entity (ie, socket connection to database) has to happen in IO.
--

settings =
    defaultConnectInfo {
        connectPort = UnixSocket "tests/redis.sock"
    }


lookupResource :: S.ByteString -> IO (Maybe S.ByteString)
lookupResource d = bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ queryResource d)


writeResource :: S.ByteString -> S.ByteString -> Redis ()
writeResource d' t' = do
    _ <- set key value      -- FIXME there's a return value; check for errors!
    return ()
  where
    key   = S.append "resource:" d'
    value = t'


storeResource :: S.ByteString -> S.ByteString -> IO ()
storeResource d' t' = bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ writeResource d' t')


--
-- For tests,
--

flushDatastore :: IO ()
flushDatastore = bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ dropResources)


dropResources :: Redis ()
dropResources = do
    _ <- flushdb
    return ()



