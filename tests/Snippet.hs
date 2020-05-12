{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-unused-imports #-}

module Main where

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Debug.Trace
import qualified Scripting.Lua as Lua

main :: IO ()
main = do
  l <- Lua.newstate
  Lua.openlibs l
  Lua.callproc l "print" ("Hello World" :: String)
  Lua.close l
