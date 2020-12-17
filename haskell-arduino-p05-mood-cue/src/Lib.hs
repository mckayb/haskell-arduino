module Lib
    ( someFunc
    ) where

import Copilot.Arduino.Uno
import Servo
import qualified Copilot.Arduino.Library.Serial as Serial

someFunc :: IO ()
someFunc = arduino $ do
    potVal <- unsafeCast <$> (input a0 :: Sketch (Behavior ADC)) :: Sketch (Behavior Float)

    Serial.baud 9600
    Serial.device =: [ Serial.str "potVal: "
                     , Serial.show potVal
                     ]
    let angle = Servo.transform potVal
    Serial.device =: [ Serial.str "angle: "
                     , Serial.show angle
                     , Serial.char '\n'
                     ]

    pin9 =: pwm' angle
    delay =: MilliSeconds (constant 15)
    