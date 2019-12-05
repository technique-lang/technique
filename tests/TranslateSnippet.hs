{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

import Core.Data
import Core.Program
import Data.DList

import Technique.Internal
import Technique.Translate
import Technique.Language
import Technique.Quantity ()
import Technique.Diagnostics ()
import ExampleProcedure hiding (main)

stubProcedure :: Step
stubProcedure = Nested empty

testEnv :: Environment
testEnv = Environment
    { environmentVariables = emptyMap
    , environmentFunctions = emptyMap
    , environmentRole = Unspecified
    , environmentAccumulated = Nested empty
    }

main :: IO ()
main = execute $ do
    let result = runTranslate testEnv $ do
            insertProcedure exampleProcedureOven
            translateProcedure exampleRoastTurkey
            -- translateExpression (Amount (Number 42))
    case result of
        Left err    -> write (renderFailure err)
        Right (x,_) -> writeR x

