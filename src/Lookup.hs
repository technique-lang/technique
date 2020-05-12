{-# LANGUAGE OverloadedStrings #-}

module Lookup (lookupResource, storeResource, flushDatastore) where

import Control.Monad.CatchIO (bracket)
import qualified Data.ByteString.Char8 as S
import Database.Redis

--
-- Store and Access keys from Redis database. If the key is unset returns
-- Nothing.
--

fromReply :: (Either Reply (Maybe S.ByteString)) -> Maybe S.ByteString
fromReply x =
  case x of
    Right sm' -> sm'
    Left (Error s') -> Just s'
    Left _ -> Just "Kaboom!\n" -- Is this even possible?

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
  defaultConnectInfo
    { connectPort = UnixSocket "tests/redis.sock"
    }

lookupResource :: S.ByteString -> IO (Maybe S.ByteString)
lookupResource d =
  bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ queryResource d)

writeResource :: S.ByteString -> S.ByteString -> Redis ()
writeResource d' t' = do
  _ <- set key value -- FIXME there's a return value; check for errors!
  return ()
  where
    key = S.append "resource:" d'
    value = t'

storeResource :: S.ByteString -> S.ByteString -> IO ()
storeResource d' t' =
  bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ writeResource d' t')

--
-- For tests,
--

flushDatastore :: IO ()
flushDatastore =
  bracket
    (connect settings)
    (\r -> runRedis r $ quit)
    (\r -> runRedis r $ dropResources)

dropResources :: Redis ()
dropResources = do
  _ <- flushdb
  return ()
