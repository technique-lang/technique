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

module HttpServer (site) where

import Prelude hiding (catch)

import Snap.Core
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (catch)
import Control.Exception (SomeException)
import System.IO (stderr, hPutStrLn, hFlush)

import Lookup (lookupResource, storeResource)

--
-- Top level URL routing logic.
--

site :: Snap ()
site = catch
        (routeRequests)
        (\e -> serveError "Splat\n" e)

routeRequests :: Snap ()
routeRequests =
        ifTop serveHome
    <|> route
            [("/resource/:id", serveResource)]
    <|> serveNotFound


serveResource :: Snap ()
serveResource = do
    r <- getRequest
    let m = rqMethod r
    case m of
        GET     -> handleGetMethod
        PUT     -> handlePutMethod
        POST    -> handlePostMethod
        _       -> serveBadRequest -- wrong! There's actually a 4xx code for this

--
-- If they request / then we send them to an info page. 
--

serveHome :: Snap ()
serveHome = do
    modifyResponse $ setContentType "text/plain"
    writeBS "Home\n"


serveNotFound :: Snap a
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setHeader "Content-Type" "text/html"
    sendFile "content/404.html"

    r <- getResponse
    finishWith r

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
        Just "application/json" -> handleAsREST
        Just "text/html"        -> handleAsBrowser
        _                       -> handleAsText


handleAsREST :: Snap ()
handleAsREST = do
    i'0 <- getParam "id"
    let i' = fromMaybe "0" i'0
    
    e' <- lookupById i'

{-
    case e'0 of
        Just e' -> e'
        Nothing -> serveNotFound
-}
    let r' = S.append e' "\n"
        l  = fromIntegral $ S.length r'

    modifyResponse $ setContentType "application/json"
    modifyResponse $ setHeader "Cache-Control" "max-age=42"
    modifyResponse $ setContentLength $ l
    writeBS r'


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
        Just "application/json" -> updateResource
        _                       -> serveUnsupported


updateResource :: Snap ()
updateResource = do
    b'' <- readRequestBody 4096
    let b' = fromLazy b''
    
    i'0 <- getParam "id"
    let i' = fromMaybe "0" i'0

    storeById i' b'
    modifyResponse $ setResponseStatus 204 "Updated" -- "No Content"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setContentLength 0
    return ()
  where
    fromLazy l'' = S.concat $ L.toChunks l''
    


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
    liftIO $ do
        hPutStrLn stderr ""
        hPutStrLn stderr cs
        hFlush stderr

--
-- Switch from Snap monad (through IO) to Redis. The Maybe return represents a
-- key with no vaue.
--

lookupById :: ByteString -> Snap ByteString
lookupById i' = do
    x'0 <- liftIO $ lookupResource i'
    case x'0 of
        Just x' -> return x'
        Nothing -> serveNotFound


storeById :: ByteString -> ByteString -> Snap ()
storeById i' x' = do
    liftIO $ storeResource i x'
  where
    i = read $ S.unpack i'

