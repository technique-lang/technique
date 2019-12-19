{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckTranslationStage
    ( checkTranslationStage
    , main
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.System
import Test.Hspec

import Technique.Builtins
import Technique.Failure
import Technique.Internal
import Technique.Language
import Technique.Translate

import ExampleProcedure hiding (main)

main :: IO ()
main = do
    finally (hspec checkTranslationStage) (putStrLn ".")

testEnv :: Environment
testEnv = Environment
    { environmentVariables = singletonMap (Identifier "x") (Name "!x")
    , environmentFunctions = insertKeyValue (Identifier "oven") (Subroutine exampleProcedureOven NoOp) builtinProcedures
    , environmentRole = Inherit
    , environmentCurrent = (0,Blank)
    , environmentAccumulated = NoOp
    }

checkTranslationStage :: Spec
checkTranslationStage = do
    describe "Invalid code emits expected compiler failures" $ do
        it "encountering undefined symbol" $
          let
            expr = Undefined
          in do
            runTranslate testEnv (translateExpression expr)
                `shouldBe` Left (EncounteredUndefined (0,Blank))

        it "encountering unknown variable" $
          let
            expr = Variable [Identifier "y"]
          in do
            runTranslate testEnv (translateExpression expr)
                `shouldBe` Left (UseOfUnknownIdentifier (0,Blank) (Identifier "y"))

        it "encountering unknown procedure" $
          let
            expr1 = Variable [Identifier "x"]
            expr2 = Application (Identifier "f") expr1
            stmt = (0, Execute expr2)
            block = Block [stmt]
            technique = emptyTechnique { techniqueBody = [ emptyProcedure { procedureBlock = block } ] }
          in do
            let result = runTranslate testEnv (translateTechnique technique)
            case result of
                Left (CallToUnknownProcedure _ (Identifier i)) ->
                    i `shouldBe` "f"
                Left _ -> fail "Incorrect CompilerFailure encountered"
                Right _ -> fail "Should have emitted CompilerFailure"

        it "expected builtin procedures are defined" $ do
            fmap functionName (lookupKeyValue (Identifier "task") (environmentFunctions testEnv))
                `shouldBe` Just (Identifier "task")

        it "encountering builtin procedure" $
          let
            expr = Application (Identifier "task") (Text "Say Hello")
            stmt = (0, Execute expr)
            block = Block [stmt]
            proc = emptyProcedure { procedureName = Identifier "hypothetical", procedureBlock = block }
            tech = emptyTechnique { techniqueBody = [ proc ]}
          in do
            let result = runTranslate testEnv (translateTechnique tech)
            case result of
                Right ([(Subroutine _ (Invocation _ (Primitive proc1 _) _))], _) ->
                    procedureName proc1 `shouldBe` Identifier "task"
                Right (((Subroutine _ step):_),_) -> fail ("Should have translated to a Primitive, step " ++ show step) -- probable that the above pattern match now needs fixing
                _ -> fail "Should have pattern matched"

        it "encounters a declaration for an already existing procedure name" $
          let
            proc = exampleProcedureOven -- already in testEnv
            stmt = (0, Declaration proc)
            block = Block [stmt]
          in do
            -- verify precondition that there is one already there
            fmap functionName (lookupKeyValue (Identifier "oven") (environmentFunctions testEnv))
                `shouldBe` Just (Identifier "oven")
            -- attempt to declare a procedure by a name already in use
            let result = runTranslate testEnv (translateBlock block)
            case result of
                Left (ProcedureAlreadyDeclared _ (Identifier i)) ->
                    i `shouldBe` "oven"
                Left _ -> fail "Incorrect CompilerFailure encountered"
                Right _ -> fail "Should have emitted CompilerFailure"
