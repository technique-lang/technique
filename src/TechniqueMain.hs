{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

import Core.Program
import Core.Text

import TechniqueUser
    ( commandCheckTechnique
    , commandFormatTechnique
    , commandSimulateTechnique
    )

version :: Version
#ifdef __GHCIDE__
version = "0"
#else
version = $(fromPackage)
#endif

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
            [ Option "raw-control-chars" (Just 'R') Empty [quote|
                Emit ANSI escape codes for syntax highlighting even if output
                is redirected to a pipe or file.
              |]
            , Argument "filename" [quote|
                The file containing the code for the procedure you want to format.
              |]
            ]
        , Command "simulate" "Evaluate a procedure in simulation mode"
            [ Argument "filename" [quote|
                The file containing the code for the procedure you want to evaluate.
              |]
            ]

        ])
    executeWith context program

program :: Program None ()
program = do
    params <- getCommandLine
    case commandNameFrom params of
        Nothing -> do
            write "Illegal state?"
            terminate 2
        Just command -> case command of
            "check"     -> commandCheckTechnique
            "format"    -> commandFormatTechnique
            "simulate"  -> commandSimulateTechnique
            _       -> do
                write "Unknown command?"
                terminate 3
    event "Done"
