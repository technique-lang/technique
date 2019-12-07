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
            void (syntaxCheck procfile)
        Cycle -> do
            -- use inotify to rebuild on changes
            forever (syntaxCheck procfile >> waitForChange [procfile])

syntaxCheck :: FilePath -> Program None ()
syntaxCheck procfile = do
    result <- loadProcedure procfile
    case result of
        Right _ -> do
            write "Ok"
        Left err -> do
            write err

{-|
Load an parse a procedure file
-}
loadProcedure :: FilePath -> Program None (Either Rope Technique)
loadProcedure procfile = do
    event "Read technique file"
    contents <- liftIO $ withFile procfile ReadMode hInput

    event "Parse technique file into Procedure(s)"

    -- This is somewhat horrible; reading into Bytes, then going through
    -- Rope to get to Text is a bit silly... except that interop was kinda
    -- the whole point of the unbeliever library. So, good? We can make
    -- this better if/when we come up with an effecient Stream Rope
    -- instance so megaparsec can use Rope directly.

    -- FIXME parse whole file not just a procedure FIXME
    let result = parse pTechnique procfile (fromRope (intoRope contents))
    case result of
        Right technique -> return (Right technique)
        Left err -> return (Left (intoRope (errorBundlePretty err)))



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

    result <- loadProcedure procfile
    case result of
        Right technique -> do
            terminal <- liftIO $ hIsTerminalDevice stdout
            case (terminal || raw) of
                True    -> writeR technique
                False   -> write (renderNoAnsi 80 technique)

        Left err -> do
            write err
