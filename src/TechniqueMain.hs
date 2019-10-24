{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Core.Program
import Core.Text

import TechniqueUser (commandCheckTechnique)

program :: Program None ()
program = do
    commandCheckTechnique

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <- configure version None (complex
        [ Command "check" "Syntax- and type-check the given procedure"
            [ Option "watch" Nothing Empty [quote|
                Watch the given procedure file and recompile if changes are detected.
              |]
            , Argument "filename" [quote|
                The file containing the code for the procedure you want to type-check.
              |]
            ]
        , Command "format" "Format the given procedure"
            [ Argument "file" [quote|
                The file containing the code for the procedure you want to format.
              |]
            ]
        ])
    executeWith context program
