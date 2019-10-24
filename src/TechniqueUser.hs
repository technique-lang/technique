{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Programs implementing front-end commands for users: check, format
-}

module TechniqueUser where

import Core.Program
import Core.Text
import Technique.Procedure ()

commandCheckTechnique :: Program None ()
commandCheckTechnique = do
    params <- getCommandLine
    
    let procfile = case lookupArgument "filename" params of
            Nothing -> error "invalid"
            Just file -> file

    writeS procfile
