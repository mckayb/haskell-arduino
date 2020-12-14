module Lib
    ( someFunc
    ) where

import Copilot.Arduino
import Copilot.Arduino.Uno
import qualified Copilot.Arduino.Library.Serial as Serial
import qualified Servo

someFunc = arduino $ do
    redSensorVal <- input a0 :: Sketch (Behavior ADC)

    Serial.baud 9600
    Serial.device =: [ Serial.str "Raw Sensor Values: \t Red: "
                     , Serial.show redSensorVal
                     , Serial.char '\n'
                     ]

    let servo = Servo.servo pin9

    led =: blinking
    delay =: MilliSeconds (constant 100)

