{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

import Core.Program
import Core.System
import Core.Text
import Data.Text (Text)
import Text.Megaparsec

import Technique.Formatter ()
import Technique.Parser

text :: Text
text = "5.9722 × 10^24 kg"

text1 =
    [quote|
{ 
    (42 )  
}
|]

text2 = "{ answer = 42 kg }" -- ;

text3 = "x"

main :: IO ()
main = execute $ do
    let !result = parse pBlock "" text1
    liftIO $ hFlush stdout
    sleep 0.25
    case result of
        Left err -> write (intoRope (errorBundlePretty err))
        Right x -> writeR x

blob1 :: Text
blob1 =
    [quote|
% technique v0
! PublicDomain
    check_sensors : Reading -> Emergency
    {
        check_sensors ()
    }
|]

blob2 :: Text
blob2 =
    [quote|
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

blob4 :: Text
blob4 =
    [quote|
% technique v0
! Super Secret Squirrel Private Not License

    f : X -> Y
{
    @chef {
[
       		     "mass" ~ 45.0x10^4 kg
                    "diameter" ~ 5 m
    ]
}}
|]
