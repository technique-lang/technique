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
import Snap.Core hiding (setHeader, setContentType)
import Snap.Test
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO)
import Test.HUnit
import Data.Maybe (fromMaybe)

import HttpServer (site)

--
-- Naming convention used is Requests as q and Responses as p.
--

type ContentType = ByteString

type AcceptType = ByteString

main :: IO Counts
main = runTestTT tests


tests = TestList
        [TestLabel "Basic routing" testRouting]


testRouting = TestList
        [testBogusUrl,
         testHomepage,
         testBasicUpdate]


testBogusUrl = TestCase $ do
    (q,p) <- makeRequest GET "/booga" "text/html"
    assert404 p

testHomepage = TestCase $ do
    (q,p) <- makeRequest GET "/" "text/html"
    assertSuccess p


testBasicUpdate = TestCase $ do
    (q,p) <- makeRequest PUT "/resource/254" "application/json"
    expectCode 204 (q,p)
    expectType "" (q,p)
    expectLength 0 (q,p)


--
-- Carry out an HTTP request, internally, creating the request out of the
-- supplied method, URL, and content type that you're willing to accept. The
-- site varable is the top level Snap handler from HttpServer, per import.
--

makeRequest :: Method -> ByteString -> AcceptType -> IO (Request, Response)
makeRequest method url' accept' = do
    q <- buildRequest request
    p <- runHandler request site
    return (q,p)
  where
    request = case method of
        GET         -> setupGetRequest url' accept'
        PUT         -> setupPutRequest url' accept'
        otherwise   -> undefined
    

setupGetRequest :: (MonadIO m) => ByteString -> AcceptType -> RequestBuilder m ()
setupGetRequest url' mime' = do
    get url' Map.empty
    setHeader "Accept" mime'


setupPutRequest :: (MonadIO m) => ByteString -> ContentType -> RequestBuilder m ()
setupPutRequest url' mime' = do
    put url' mime' body'
  where
    body' = "This is a test"


expectCode :: Int -> (Request, Response) -> Assertion
expectCode i (q,p) = do
    assertEqual msg i code
  where
    code = rspStatus p
    msg = summarize (q,p)

expectType :: ContentType -> (Request, Response) -> Assertion
expectType t' (q,p) = do
    assertEqual msg t' mime'
  where
    mime'0 = getHeader "Content-Type" p
    mime'  = fromMaybe "" mime'0
    msg   = summarize (q,p)


expectLength :: Int -> (Request, Response) -> Assertion
expectLength i (q,p) = do
    assertBool (msg ++ "\nBut no Content-Length header!") (len > -1)
    assertEqual msg i len
  where
    len'0 = getHeader "Content-Length" p
    len'  = fromMaybe "-1" len'0
    len   = read $ S.unpack len'
    msg   = summarize (q,p)


--
-- Summarize a Request and Response pair, used for output from HUnit when an
-- assertion fails.
--

summarize :: (Request, Response) -> String
summarize (q,p) =
    ">>> " ++ method ++ " " ++ uri ++ "\n<<< " ++ code ++ " " ++ label
  where
    method = show $ rqMethod q
    uri = S.unpack $ rqURI q
    code = show $ rspStatus p
    label = S.unpack $ rspStatusReason p
    

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

