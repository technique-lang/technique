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

lookupTarget :: S.ByteString -> Snap S.ByteString
lookupTarget x' = catch
    (liftIO $ lookupTemp x)
    (\e -> do
        serveError x' e
        return "")
  where
    x = read $ S.unpack x'


handleGetMethod :: Snap ()
handleGetMethod = do
	modifyResponse $ setContentType "application/json"
	modifyResponse $ setHeader "Cache-Control" "max-age=1"
	sendFile "hello.js"


handlePutMethod = undefined
handlePostMethod = undefined

serveResource :: Snap ()
serveResource = do
    r <- getRequest
    let m = rqMethod r
    case m of
        GET     -> handleGetMethod
        PUT     -> handlePutMethod
        POST    -> handlePostMethod
        otherwise -> serveBadRequest -- wrong! There's actually a 4xx code for this


serveError :: S.ByteString -> SomeException -> Snap ()
serveError x e = do
    logError msg
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS "500 Internal Server Error\n"
    r <- getResponse
    finishWith r
  where
    msg = S.pack $ show (e :: SomeException)


serveNotFound :: Snap ()
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    sendFile "content/404.html"

serveBadRequest :: Snap ()
serveBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "400 Bad Request\n"



--
-- If they request / then we send them to an info page. 
--

serveHome :: Snap ()
serveHome = do
    writeBS "Home\n"

--
-- Top level URL routing logic.
--

site :: Snap ()
site = route
    [("/", serveHome),
     ("/resource/:id", serveResource)]
    <|> serveNotFound

main :: IO ()
main = quickHttpServe site

