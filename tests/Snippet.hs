{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module Main where

import qualified Scripting.Lua as Lua

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Blaze.ByteString.Builder (Builder)
import Data.ByteString (ByteString)
import Debug.Trace

import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.callproc l "print" ("Hello World" :: String)
    Lua.close l


