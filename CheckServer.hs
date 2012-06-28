--
-- Procedures
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
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

import Prelude hiding (catch)

import Snap.Http.Server
import Snap.Core (Snap, Request, Response, Method(..))
import Snap.Test
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO)
import Test.HUnit

import HttpServer (site)


main :: IO Counts
main = runTestTT tests


tests = TestList
        [TestLabel "Basic routing" testRouting]


testRouting = TestList
        [testBogusUrl,
         testHomepage]


testBogusUrl = TestCase $ do
    res <- makeRequest GET "/booga" "text/html"
    assert404 res

testHomepage = TestCase $ do
    res <- makeRequest GET "/" "text/html"
    assertSuccess res



makeRequest :: Method -> ByteString -> ByteString -> IO Response
makeRequest method url' accept' = do
    res <- runHandler req site
    return res
  where
    req = case method of
        GET         -> setupGetRequest url' accept'
        PUT         
        otherwise   -> undefined
    

setupGetRequest :: (MonadIO m) => ByteString -> ByteString -> RequestBuilder m ()
setupGetRequest url' mime' = do
    get url' Map.empty
    setHeader "Accept" mime'




example1 :: (MonadIO m) => RequestBuilder m ()
example1 = do
    get "/headers" Map.empty
    setHeader "Accept" "application/xml"
    setContentType "text/html"


example2 :: (MonadIO m) => RequestBuilder m ()
example2 = do
    put "/bonus" "text/plain" body
  where
    body = "This is a test"

