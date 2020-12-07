module Lib where

import Copilot.Arduino.Uno


someFunc = arduino $ do
    switchState <- input pin2 :: Sketch (Behavior Bool)

    if switchState == false then do
        pin3 =: true
        pin4 =: false
        pin5 =: false
    else do
        pin3 =: false
        pin4 =: false
        pin5 =: true
        delay =: MilliSeconds 250

        pin4 =: true
        pin5 =: false
        delay =: MilliSeconds 250
