{-# LANGUAGE OverloadedStrings #-}

import Core.Program

-- import Technique.Evaluator
import Technique.Failure
import Technique.Formatter ()
import Technique.Internal
import TechniqueUser

main :: IO ()
main = do
    context <-
        configure
            "0"
            None
            ( simple
                [ Argument "filename" "Procedure you want to evaluate."
                ]
            )

    executeWith context $ do
        setVerbosityLevel Debug
        params <- getCommandLine
        let filename = case lookupArgument "filename" params of
                Nothing -> error "needed filename!"
                Just f -> f

        surface <- loadTechnique filename
        let source =
                emptySource
                    { sourceFilename = filename
                    , sourceContents = surface
                    }
        concrete <- parsingPhase source
        abstract <- translationPhase source concrete
        final <- evaluationPhase abstract Unitus

        debugR "final" final
