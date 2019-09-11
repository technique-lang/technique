{-# LANGUAGE OverloadedStrings #-}

import Core.Program
import Technique.Procedure ()

main :: IO ()
main = execute $ do
    write "Start"