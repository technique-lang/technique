{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Programs implementing front-end commands for users: check, format
-}

module TechniqueUser where

import Core.Program
import Core.Text
import Core.System
import Technique.Procedure ()
import Technique.Parser

import Text.Megaparsec

commandCheckTechnique :: Program None ()
commandCheckTechnique = do
    params <- getCommandLine
    
    let procfile = case lookupArgument "filename" params of
            Nothing -> error "invalid"
            Just file -> file

    event "Read procedure file"
    contents <- liftIO $ withFile procfile ReadMode hInput

    event "Parse procedure file into Procedure"

    -- This is somewhat horrible; reading into Bytes, then going through
    -- Rope to get to Text is a bit silly... except that interop was kinda
    -- the whole point of the unbeliever library. So, good? We can make
    -- this better if/when we come up with an effecient Stream Rope
    -- instance so megaparsec can use Rope directly.

    let result = parse pProcedure procfile (fromRope (intoRope contents))
    case result of
        Right _ -> do
            write "Ok"
        Left err -> do
            write (intoRope (errorBundlePretty err))
            terminate 42


commandFormatTechnique :: Program None ()
commandFormatTechnique = do
    write "Not yet implemented, sorry"
    terminate 42
