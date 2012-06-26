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

module Technique (site) where

import Prelude hiding (catch)

import Snap.Http.Server
import Snap.Core
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Numeric
import Data.Char
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (catch, throw)
import Control.Exception (SomeException)

import Lookup (lookupTemp)

--
-- Top level URL routing logic.
--

site :: Snap ()
site = catch
        (routeRequests)
        (\e -> serveError "Splat\n" e)

routeRequests :: Snap ()
routeRequests = route
    [("/", serveHome),
     ("/resource/:id", serveResource)]
    <|> serveNotFound


serveResource :: Snap ()
serveResource = do
    r <- getRequest
    let m = rqMethod r
    case m of
        GET     -> handleGetMethod
        PUT     -> handlePutMethod
        POST    -> handlePostMethod
        otherwise -> serveBadRequest -- wrong! There's actually a 4xx code for this

--
-- If they request / then we send them to an info page. 
--

serveHome :: Snap ()
serveHome = do
    writeBS "Home\n"


serveNotFound :: Snap ()
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    sendFile "content/404.html"


serveBadRequest :: Snap ()
serveBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "400 Bad Request\n"


--
-- Dispatch normal GET requests based on MIME type.
--

handleGetMethod :: Snap ()
handleGetMethod = do
    r <- getRequest
    let mime0 = getHeader "Accept" r

    case mime0 of
        Just "application/json"  -> handleAsREST
        Just "text/html"         -> handleAsBrowser
        otherwise                -> handleAsText


handleAsREST :: Snap ()
handleAsREST = do
    modifyResponse $ setContentType "application/json"
    modifyResponse $ setHeader "Cache-Control" "max-age=1"
    sendFile "hello.js"


handleAsBrowser :: Snap ()
handleAsBrowser = do
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Cache-Control" "max-age=1"
    sendFile "hello.html"


handleAsText :: Snap ()
handleAsText = do
    modifyResponse $ setContentType "text/plain"
    writeBS "Sounds good to me\n"


--
-- Create a new procedures
--


handlePostMethod :: Snap ()
handlePostMethod = do
    modifyResponse $ setResponseStatus 201 "Created"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setHeader "Location" "http://server.example.com/something/788"



--
-- Given an correctly addressed procedure, update it with the inbound entity.
--

handlePutMethod :: Snap ()
handlePutMethod = do
    r <- getRequest
    let mime0 = getHeader "Content-Type" r

    case mime0 of
        Just "application/json"  -> updateResource
        otherwise                -> serveUnsupported


updateResource :: Snap ()
updateResource = do
    body <- readRequestBody 4096
    modifyResponse $ setResponseStatus 204 "Updated" -- "No Content"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    return ()


serveUnsupported :: Snap ()
serveUnsupported = do
    modifyResponse $ setResponseStatus 415 "Unsupported Media Type"
    writeBS "415 Unsupported Media Type\n"
    r <- getResponse
    finishWith r


--
-- The exception will be dumped to the server's stdout, while the supplied
-- message will be sent out with the response (ideally only for debugging
-- purposes, but easier than looking in log/error.log for details). 
--

serveError :: ByteString -> SomeException -> Snap ()
serveError x' e = do
    debug msg
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS x'
    r <- getResponse
    finishWith r
  where
    msg = show (e :: SomeException)


debug :: String -> Snap ()
debug cs = do
    liftIO $ putStrLn cs 


debug' :: ByteString -> Snap ()
debug' x' = do
    debug $ S.unpack x'


--
-- Placeholder
--

lookupTarget :: ByteString -> Snap ByteString
lookupTarget x' = catch
    (liftIO $ lookupTemp x)
    (\e -> do
        serveError x' e
        return "")
  where
    x = read $ S.unpack x'


