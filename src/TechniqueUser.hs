{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Programs implementing front-end commands for users: check, format
-}

module TechniqueUser where

import Control.Monad (forever)
import Core.Program
import Core.Text
import Core.System
import System.IO (hIsTerminalDevice)
import Text.Megaparsec (parse, errorBundlePretty)

import Technique.Diagnostics ()
import Technique.Failure
import Technique.Formatter ()
import Technique.Internal
import Technique.Language
import Technique.Parser
import Technique.Translate

data Mode = Cycle | Once

commandCheckTechnique :: Program None ()
commandCheckTechnique = do
    params <- getCommandLine
    
    let procfile = case lookupArgument "filename" params of
            Just file   -> file
            _           -> error "Invalid State"

    let mode = case lookupOptionFlag "watch" params of
            Just True   -> Cycle
            Nothing     -> Once
            _           -> error "Invalid State"

    case mode of
        Once -> do
            -- normal operation, single pass
            code <- syntaxCheck procfile
            terminate code
        Cycle -> do
            -- use inotify to rebuild on changes
            forever (syntaxCheck procfile >> waitForChange [procfile])

syntaxCheck :: FilePath -> Program None Int
syntaxCheck procfile =
    catch
        (do
            surface <- loadTechnique procfile
            concrete <- parsingPhase procfile surface
            abstract <- translationPhase concrete
            debugR "abstract" abstract
            write "ok"
            return 0)
        (\e -> do
            write ("failed: " <> renderFailure e)
            return (fromEnum e))

{-|
Load a technique file hopefully containing a procedure.
-}
loadTechnique :: FilePath -> Program None Bytes
loadTechnique filename = do
    event "Read technique file"

    -- This is somewhat horrible; reading into Bytes, then going through
    -- Rope to get to Text is a bit silly... except that interop was kinda
    -- the whole point of the unbeliever library. So, good? We can make
    -- this better if/when we come up with an effecient Stream Rope
    -- instance so megaparsec can use Rope directly.

    contents <- liftIO $ withFile filename ReadMode hInput
    return (intoBytes contents)

{-|
Parse technique content into a concrete syntax object.
-}
parsingPhase :: FilePath -> Bytes -> Program None Technique
parsingPhase filename bytes = do
    event "Parse into Procedure(s)"

    let result = parse pTechnique filename (fromRope (intoRope bytes))
    case result of
        Right technique -> return technique
        Left err -> throw (ParsingFailed (errorBundlePretty err))

{-|
Take a static Procedure definition and spin it up into a sequence of
"Subroutine" suitable for interpretation. In other words, translate between
the concrete syntax types and the abstract syntax we can feed to an
evaluator.
-}
-- FIXME better return type
translationPhase :: Technique -> Program None [Subroutine]
translationPhase technique =
  let
    env = emptyEnvironment
    result = runTranslate env (translateTechnique technique)
  in
    case result of
        Left failure -> do
            throw failure
        Right (xs,_) -> do
            return xs



commandFormatTechnique :: Program None ()
commandFormatTechnique = do
    params <- getCommandLine

    let raw = case lookupOptionFlag "raw-control-chars" params of
            Nothing     -> False
            Just True   -> True
            _           -> error "Invalid State"

    let procfile = case lookupArgument "filename" params of
            Just file   -> file
            _           -> error "Invalid State"

    catch
        (do
            surface <- loadTechnique procfile
            technique <- parsingPhase procfile surface

            terminal <- liftIO $ hIsTerminalDevice stdout
            case (terminal || raw) of
                True    -> writeR technique
                False   -> write (renderNoAnsi 80 technique))
        (\e -> do
            write ("failed: " <> renderFailure e)
            terminate (fromEnum e))