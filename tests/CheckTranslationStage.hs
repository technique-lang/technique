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
testEnv = emptyEnvironment
    { environmentVariables = singletonMap (Identifier "x") (Name "!x")
    , environmentFunctions = insertKeyValue (Identifier "oven") (Subroutine exampleProcedureOven NoOp) builtinProcedures
    }

checkTranslationStage :: Spec
checkTranslationStage = do
    describe "Invalid code emits expected compiler failures" $ do
        it "encountering undefined symbol" $
          let
            expr = Undefined 0
            result = runTranslate testEnv (translateExpression expr)
          in do
            case result of
                Left (CompilationError _ failure) -> do
                    failure `shouldBe` EncounteredUndefined
                Right _ -> fail "Should have emitted CompilerFailure"

        it "encountering unknown variable" $
          let
            expr = Variable 0 [Identifier "y"]
            result = runTranslate testEnv (translateExpression expr)
          in do
            case result of
                Left (CompilationError _ failure) ->
                    failure `shouldBe` UseOfUnknownIdentifier (Identifier "y")
                Right _ -> fail "Should have emitted CompilerFailure"

        it "encountering unknown procedure" $
          let
            expr1 = Variable 0 [Identifier "x"]
            expr2 = Application 0 (Identifier "f") expr1
            stmt = Execute 0 expr2
            block = Block [stmt]
            technique = emptyTechnique { techniqueBody = [ emptyProcedure { procedureBlock = block } ] }
          in do
            let result = runTranslate testEnv (translateTechnique technique)
            case result of
                Left (CompilationError _ (CallToUnknownProcedure (Identifier i))) ->
                    i `shouldBe` "f"
                Left _ -> fail "Incorrect CompilerFailure encountered"
                Right _ -> fail "Should have emitted CompilerFailure"

        it "expected builtin procedures are defined" $ do
            fmap functionName (lookupKeyValue (Identifier "task") (environmentFunctions testEnv))
                `shouldBe` Just (Identifier "task")

        it "encountering builtin procedure" $
          let
            expr = Application 0 (Identifier "task") (Text 5 "Say Hello")
            stmt = Execute 0 expr
            block = Block [stmt]
            proc = emptyProcedure { procedureName = Identifier "hypothetical", procedureBlock = block }
            tech = emptyTechnique { techniqueBody = [ proc ]}
          in do
            let result = runTranslate testEnv (translateTechnique tech)
            case result of
                Right ([(Subroutine _ (Invocation _ _ (Primitive proc1 _) _))], _) ->
                    procedureName proc1 `shouldBe` Identifier "task"
                Right (((Subroutine _ step):_),_) -> fail ("Should have translated to a Primitive, step " ++ show step) -- probable that the above pattern match now needs fixing
                _ -> fail "Should have pattern matched"

        it "encounters a declaration for an already existing procedure name" $
          let
            proc = exampleProcedureOven -- already in testEnv
            stmt = Declaration 0 proc
            block = Block [stmt]
          in do
            -- verify precondition that there is one already there
            fmap functionName (lookupKeyValue (Identifier "oven") (environmentFunctions testEnv))
                `shouldBe` Just (Identifier "oven")
            -- attempt to declare a procedure by a name already in use
            let result = runTranslate testEnv (translateBlock block)
            case result of
                Left (CompilationError _ (ProcedureAlreadyDeclared (Identifier i))) ->
                    i `shouldBe` "oven"
                Left _ -> fail "Incorrect CompilerFailure encountered"
                Right _ -> fail "Should have emitted CompilerFailure"
