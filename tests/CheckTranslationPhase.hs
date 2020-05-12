{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckTranslationPhase
  ( checkTranslationPhase,
    main,
  )
where

import Core.Data.Structures
import Core.System
import Core.Text.Rope ()
import ExampleProcedure hiding (main)
import Technique.Builtins
import Technique.Failure
import Technique.Internal
import Technique.Language
import Technique.Translate
import Test.Hspec

main :: IO ()
main = do
  finally (hspec checkTranslationPhase) (putStrLn ".")

testEnv :: Environment
testEnv =
  emptyEnvironment
    { environmentVariables = singletonMap (Identifier "x") (Name "!x"),
      environmentFunctions = insertKeyValue (Identifier "oven") (Subroutine exampleProcedureOven NoOp) builtinProcedures
    }

simpleProcedure :: Procedure
simpleProcedure =
  emptyProcedure
    { procedureName = Identifier "mock",
      procedureParams = [Identifier "a", Identifier "b"],
      procedureInput = [Type "OneThing", Type "Another"],
      procedureOutput = [Type "Stuff"],
      procedureBlock = Block [Execute 0 (Variable 0 [Identifier "a", Identifier "b"])]
    }

checkTranslationPhase :: Spec
checkTranslationPhase = do
  describe "Invalid code emits expected compiler failures" $ do
    it "encountering undefined symbol" $
      let expr = Undefined 0
          result = runTranslate testEnv (translateExpression expr)
       in do
            case result of
              Left (CompilationError _ failure) -> do
                failure `shouldBe` EncounteredUndefined
              Right _ -> fail "Should have emitted CompilerFailure"

    it "encountering unknown variable" $
      let expr = Variable 0 [Identifier "y"]
          result = runTranslate testEnv (translateExpression expr)
       in do
            case result of
              Left (CompilationError _ failure) ->
                failure `shouldBe` UseOfUnknownIdentifier (Identifier "y")
              Right _ -> fail "Should have emitted CompilerFailure"

    it "encountering unknown procedure" $
      let expr1 = Variable 0 [Identifier "x"]
          expr2 = Application 0 (Identifier "f") expr1
          stmt = Execute 0 expr2
          block = Block [stmt]
          technique = emptyTechnique {techniqueBody = [emptyProcedure {procedureBlock = block}]}
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
      let expr = Application 0 (Identifier "task") (Text 5 "Say Hello")
          stmt = Execute 0 expr
          block = Block [stmt]
          proc = emptyProcedure {procedureName = Identifier "hypothetical", procedureBlock = block}
          tech = emptyTechnique {techniqueBody = [proc]}
       in do
            let result = runTranslate testEnv (translateTechnique tech)
            case result of
              Right ([(Subroutine _ (Invocation _ _ (Primitive proc1 _) _))], _) ->
                procedureName proc1 `shouldBe` Identifier "task"
              Right (((Subroutine _ step) : _), _) -> fail ("Should have translated to a Primitive, step " ++ show step) -- probable that the above pattern match now needs fixing
              _ -> fail "Should have pattern matched"

    it "encounters a declaration for an already existing procedure name" $
      let proc = exampleProcedureOven -- already in testEnv
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

  {-
  Having made it past the various things that should throw CompilationErrors,
  we now run through some examples which should parse and translate correctly
  to full abstract syntax trees.
  -}

  describe "Known Procedure is translated to correct abstract syntax tree" $ do
    it "multiple params of a procedure are in scope as variables" $
      let result = runTranslate testEnv (translateProcedure simpleProcedure)
       in do
            case result of
              Right (func, _) ->
                func
                  `shouldBe` Subroutine
                    simpleProcedure
                    (Tuple 0 [Depends 0 (Name "!a"), Depends 0 (Name "!b")])
              _ -> fail "Should have translated procedure"
