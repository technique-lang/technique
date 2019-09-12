{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Core.Program
import Technique.Procedure ()

program :: Program None ()
program = do
    write "Start"

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <- configure version None (simple
        [
        ])
    executeWith context program