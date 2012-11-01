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

module CheckServer (spec) where

import Prelude hiding (catch)

import Snap.Core hiding (setHeader, setContentType, method)
import Snap.Test
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO)
import Test.HUnit
import Test.Hspec (Spec, describe, it)
import Data.Maybe (fromMaybe, fromJust)

import Utilities (assertMaybe)
import HttpServer (site)

--
-- Naming convention used is Requests as q and Responses as p.
--

type URL = ByteString

type ContentType = ByteString

type AcceptType = ByteString

spec :: Spec
spec = do
    describe "HTTP server" $ do
        testBogusUrl
        testHomepage
        testBasicRequest
        testBasicRequestContent
        testNonexistentResource
        testNonexistentSubresource
        testSubresourceContent
        testWrongMedia
        testBasicUpdate
        testResultOfUpdate


testBogusUrl =
    it "rejects a request for a bogus URL, responding 404" $ do
        (_,p) <- makeRequest GET "/booga" "text/html" Nothing
        assert404 p

testHomepage =
    it "accepts request for homepage, responding 200" $ do
        (_,p) <- makeRequest GET "/" "text/html" Nothing
        assertSuccess p

testBasicRequest =
    it "accepts request for a known good resource, responding 200" $ do
        (_,p) <- makeRequest GET "/resource/254" "application/json" Nothing
        assertSuccess p

testBasicRequestContent =
    it "accepts request for a known good resource, and its content is correct" $ do
        (q,p) <- makeRequest GET "/resource/254" "application/json" Nothing
        expectCode 200 (q,p)
        expectBody "president" (q,p)

testNonexistentResource =
    it "rejects request a non-existent resource, responding 404" $ do
        (q,p) <- makeRequest GET "/resource/a9s1t$y9e" "application/json" Nothing
        expectCode 404 (q,p)

testNonexistentSubresource =
    it "rejects request a non-existent sub-resource under valid resource" $ do
        (q,p) <- makeRequest GET "/resource/254/config" "application/json" Nothing
        expectCode 404 (q,p)

testSubresourceContent =
    it "accepts request for a valid sub-resource" $ do
        (q,p) <- makeRequest GET "/resource/42/config" "application/json" Nothing
        expectCode 200 (q,p)
        expectBody "[null]" (q,p)


testWrongMedia =
    it "rejects update via PUT with wrong media type, responding 415" $ do
        (q,p) <- makeRequest PUT "/resource/254" "application/xml" (Just "<html/>")
        expectCode 415 (q,p)

testBasicUpdate =
    it "accepts update via PUT, responding 204" $ do
        (q,p) <- makeRequest PUT "/resource/254" "application/json" body
        expectCode 204 (q,p)
        expectType "" (q,p)
        expectLength 0 (q,p)
  where
    body = Just "{\"president\": \"Regan\"}\n"

-- this will be combined into above test case
testResultOfUpdate =
    it "returns up to date represenations" $ do
        (q,p) <- makeRequest GET "/resource/254" "application/json" Nothing
        expectBody "Regan" (q,p)

testBasicUpdate2 :: Spec
testBasicUpdate2 = do
    describe "accepts update via PUT" $ do
        it "responds 204" $ do
            (q,p) <- makeRequest PUT "/resource/254" "application/json" Nothing
            expectCode 204 (q,p)
        it "has no Content-Type" $ do
            (q,p) <- makeRequest PUT "/resource/254" "application/json" Nothing
            expectType "" (q,p)
        it "has Content-Length 0" $ do
            (q,p) <- makeRequest PUT "/resource/254" "application/json" Nothing
            expectLength 0 (q,p)


--
-- Carry out an HTTP request, internally, creating the request out of the
-- supplied method, URL, and content type that you're willing to accept. The
-- site varable is the top level Snap handler from HttpServer, per import.
--

makeRequest :: Method -> URL -> AcceptType -> Maybe ByteString -> IO (Request, Response)
makeRequest method url' mime' payload'0 = do
    q <- buildRequest request
    p <- runHandler request handler
    return (q,p)
  where
    payload' = fromMaybe "" payload'0
    request = case method of
        GET         -> setupGetRequest url' mime'
        PUT         -> setupPutRequest url' mime' payload'
        _           -> undefined   
    handler = HttpServer.site


--
-- Create a GET request, including an Accept header of the given MIME type.
--

setupGetRequest :: (MonadIO m) => URL -> AcceptType -> RequestBuilder m ()
setupGetRequest url' mime' = do
    get url' Map.empty
    setHeader "Accept" mime'

--
-- Create a PUT request, specifying the MIME type of the payload.
--

setupPutRequest :: (MonadIO m) => URL -> ContentType -> ByteString -> RequestBuilder m ()
setupPutRequest url' mime' payload' = do
    put url' mime' payload'

--
-- Utility functions for test cases. Depending on the activity, we can require
-- a specific status code, content type, and so on. These functions wrap calls
-- to assert.
--

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
    msg    = summarize (q,p)


expectLength :: Int -> (Request, Response) -> Assertion
expectLength i (q,p) = do
    assertMaybe (msg ++ "But no Content-Length header!") len'0
    assertEqual (msg ++ "Content-Length header wrong") i len
  where
    len'0 = getHeader "Content-Length" p
    len'  = fromJust len'0
    len   = read $ S.unpack len'
    msg   = summarize (q,p)

expectBody :: ByteString -> (Request, Response) -> Assertion
expectBody str' (q,p) = do
    body' <- getResponseBody p
    assertBool (msg ++ "Body doesn't match") (find str' body')
  where
    msg  = summarize (q,p)
    find n' h' = not $ S.null $ snd $ S.breakSubstring n' h'


--
-- Summarize a Request and Response pair, used for output from HUnit when an
-- assertion fails.
--

summarize :: (Request, Response) -> String
summarize (q,p) =
    ">>> " ++ method ++ " " ++ uri ++ "\n<<< " ++ code ++ " " ++ label ++ "\n"
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

