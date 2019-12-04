{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

import Core.Data
import Core.Program
import Data.DList (empty)
import Data.Text (Text)
import Text.Megaparsec hiding (empty)

import Technique.Internal
import Technique.Translate
import Technique.Language
import ExampleProcedure hiding (main)

stubProcedure :: Step
stubProcedure = Sequence empty

testEnv :: Environment
testEnv = Environment
    { environmentVariables = emptyMap
    , environmentFunctions = singletonMap (Identifier "oven") (Subroutine exampleProcedureOven stubProcedure)
    , environmentRole = Unspecified
    , environmentAccumulated = Sequence empty
    }

main :: IO ()
main = execute $ do
    let !result = translate testEnv exampleRoastTurkey
    case result of
        Left err    -> write (renderFailure err)
        Right x     -> writeS x

