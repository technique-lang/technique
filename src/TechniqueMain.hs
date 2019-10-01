{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Core.Program
import Core.Text
import Technique.Procedure ()

program :: Program None ()
program = do
    write "Start"

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <- configure version None (complex
        [ Command "check" "Syntax- and type-check the given procedure."
            [ Option "watch" Nothing Empty [quote|
                Watch the given procedure file and recompile if changes are detected.
              |]
            , Argument "procfile" [quote|
                The file containing the code for the procedure you want to type-check.
              |]
            ]
        ])
    executeWith context program