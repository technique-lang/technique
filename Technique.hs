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

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route
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


handleAsREST = do
    modifyResponse $ setContentType "application/json"
    modifyResponse $ setHeader "Cache-Control" "max-age=1"
    sendFile "hello.js"

handleAsBrowser = do
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Cache-Control" "max-age=1"
    sendFile "hello.html"

handleAsText = do
    modifyResponse $ setContentType "text/plain"
    writeBS "Sounds good to me\n"

handlePutMethod = undefined
handlePostMethod = undefined



serveError :: S.ByteString -> SomeException -> Snap ()
serveError x e = do
    logError msg
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS "500 Internal Server Error\n"
    r <- getResponse
    finishWith r
  where
    msg = S.pack $ show (e :: SomeException)






--
-- Placeholder
--

lookupTarget :: S.ByteString -> Snap S.ByteString
lookupTarget x' = catch
    (liftIO $ lookupTemp x)
    (\e -> do
        serveError x' e
        return "")
  where
    x = read $ S.unpack x'


