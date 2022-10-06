{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Programs implementing front-end commands for users: check, format
-}
module TechniqueUser where

import Control.Monad (forever)
import Core.Program
import Core.System
import Core.Text
import System.IO (hIsTerminalDevice)
import Technique.Builtins
import Technique.Diagnostics ()
import Technique.Failure
import Technique.Formatter ()
import Technique.Internal
import Technique.Language
import Technique.Parser
import Technique.Translate
import Text.Megaparsec (parse)

data Mode = Cycle | Once

commandCheckTechnique :: Program None ()
commandCheckTechnique = do
    procfile <- queryArgument "filename" >>= pure . fromRope

    mode <-
        queryOptionFlag "watch" >>= \case
            True -> pure Cycle
            False -> pure Once

    case mode of
        Once -> do
            -- normal operation, single pass
            code <- syntaxCheck procfile
            terminate code
        Cycle -> do
            -- use inotify to rebuild on changes
            forever (syntaxCheck procfile >> waitForChange [procfile] >> writeR Reload)

syntaxCheck :: FilePath -> Program None Int
syntaxCheck procfile =
    catch
        ( do
            surface <- loadTechnique procfile
            let source =
                    emptySource
                        { sourceFilename = procfile
                        , sourceContents = surface
                        }
            concrete <- parsingPhase source
            abstract <- translationPhase source concrete
            debugR "abstract" abstract
            -- TODO extractionPhase
            writeR Ok
            return 0
        )
        ( \(e :: CompilationError) -> do
            writeR (Failed e)
            return (exitCodeFor e)
        )

{- |
Load a technique file hopefully containing a procedure.
-}
loadTechnique :: FilePath -> Program None Rope
loadTechnique filename = do
    info "Read source from technique file"

    -- This is somewhat horrible; reading into Bytes, then going through
    -- Rope to get to Text is a bit silly... except that interop was kinda
    -- the whole point of the unbeliever library. So, good? We can make
    -- this better if/when we come up with an effecient Stream Rope
    -- instance so megaparsec can use Rope directly.

    bytes <- liftIO $ withFile filename ReadMode hInput
    let contents = intoRope bytes
    return contents

{- |
Parse technique content into a concrete syntax object.
-}
parsingPhase :: Source -> Program None Technique
parsingPhase source = do
    info "Parse surface language into concrete Procedures"
    let contents = sourceContents source

    let result = parse pTechnique "" (fromRope contents)

    case result of
        Right technique -> return technique
        Left bundle -> throw (extractErrorBundle source bundle)

{- |
Take a static Procedure definition and spin it up into a sequence of
"Subroutine" suitable for interpretation. In other words, translate between
the concrete syntax types and the abstract syntax we can feed to an evaluator.
-}

-- FIXME better return type
translationPhase :: Source -> Technique -> Program None [Function]
translationPhase source technique =
    let env0 =
            emptyEnvironment
                { environmentFunctions = builtinProcedures
                , environmentSource = source
                }
        result = runTranslate env0 (translateTechnique technique)
     in do
            info "Translate Procedures into abstract Subroutines"
            case result of
                Left failure -> do
                    throw failure
                Right (xs, _) -> do
                    return xs

commandFormatTechnique :: Program None ()
commandFormatTechnique = do
    raw <- queryOptionFlag "raw-control-chars"

    procfile <- queryArgument "filename" >>= pure . fromRope

    catch
        ( do
            surface <- loadTechnique procfile
            let source =
                    emptySource
                        { sourceFilename = procfile
                        , sourceContents = surface
                        }

            technique <- parsingPhase source

            terminal <- liftIO $ hIsTerminalDevice stdout
            case (terminal || raw) of
                True -> writeR technique
                False -> write (renderNoAnsi 80 technique)
        )
        ( \(e :: CompilationError) -> do
            write ("failed: " <> render 78 e)
            terminate (exitCodeFor e)
        )
