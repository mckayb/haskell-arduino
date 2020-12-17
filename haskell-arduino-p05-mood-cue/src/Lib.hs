module Lib
    ( someFunc
    ) where

import Copilot.Arduino.Uno
import Servo
import qualified Copilot.Arduino.Library.Serial as Serial

someFunc :: IO ()
someFunc = arduino $ do
    potVal <- cast <$> (input a0 :: Sketch (Behavior ADC)) :: Sketch (Behavior Int32)

    Serial.baud 9600
    Serial.device =: [ Serial.str "potVal: "
                     , Serial.show potVal
                     ]
    -- TODO: Figure out why do I need to map this again to values that are close to 60 and 245?
    let angle = Servo.intTransform' 0 1023 60 245 potVal

    Serial.device =: [ Serial.str "angle: "
                     , Serial.show angle
                     , Serial.char '\n'
                     ]

    pin9 =: pwm' angle
    delay =: MilliSeconds (constant 15)
    