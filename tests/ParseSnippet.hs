{-# LANGUAGE QuasiQuotes #-}

import Core.Program
import Core.Text
import Core.System
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Megaparsec

import Technique.Parser
import Technique.Formatter ()


blob1 :: Text
blob1 = [quote|
% technique v0
! PublicDomain
    check_sensors : Reading -> Emergency
    {
        check_sensors ()
    }
|]

blob2 :: Text
blob2 = [quote|
% technique v0
! PublicDomain

Immediate Action
----------------

This is a test of the Emergency Broadcast System. Do not
be alarmed by the alarm you hear.

    sound_alarm d : Detection -> Alarm
    {
        task "Sound alarm"
    }

    activate_crisis_management a : Alarm -> Emergency
    {
        task "Sound alarm"
    }


Aftermath
---------

    restore_normalcy e : Emergency -> Peace
    {
        ?
    }
|]


main :: IO ()
main = execute $ do
    blob3 <- liftIO $ T.readFile "tests/Stub.t"
    let result = parse pTechnique "" blob3
    sleep 0.1
    case result of
        Left err    -> write (intoRope (errorBundlePretty err))
        Right x     -> writeR x
        