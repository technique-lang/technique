{-# LANGUAGE OverloadedStrings #-}

import Core.Program

import Technique.Failure
import Technique.Formatter ()

import TechniqueUser

main :: IO ()
main = do
    context <- configure "0" None (simple
        [ Argument "filename" "The file containing the code for the procedure you want to type-check."
        ])
    
    executeWith context $ do
        params <- getCommandLine
        let filename = case lookupArgument "filename" params of
                Nothing -> error "needed filename!"
                Just f -> f

        surface <- loadTechnique filename
        let source = emptySource
                { sourceFilename = filename
                , sourceContents = surface
                }
        concrete <- parsingPhase source
        abstract <- translationPhase source concrete
        writeR abstract
