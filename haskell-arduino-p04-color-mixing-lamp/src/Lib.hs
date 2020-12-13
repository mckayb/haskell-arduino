{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( someFunc
    ) where

import Data.Function (const)
import Control.Monad.Writer (MonadWriter(tell))
import Copilot.Arduino.Uno
import Copilot.Arduino.Internals
import qualified Copilot.Arduino.Library.Serial as Serial

someFunc :: IO ()
someFunc = arduino $ do
    redSensorVal <- unsafeCast <$> (input a0 :: Sketch (Behavior ADC)) :: Sketch (Behavior Float)
    delay =: MilliSeconds (constant 5)
    greenSensorVal <- unsafeCast <$> (input a1 :: Sketch (Behavior ADC)) :: Sketch (Behavior Float)
    delay =: MilliSeconds (constant 5)
    blueSensorVal <- unsafeCast <$> (input a2 :: Sketch (Behavior ADC)) :: Sketch (Behavior Float)
    delay =: MilliSeconds (constant 5)

    Serial.baud 9600
    Serial.device =: [ Serial.str "Raw Sensor Values: \t Red: "
                     , Serial.show redSensorVal
                     , Serial.str "\t Green: "
                     , Serial.show greenSensorVal
                     , Serial.str "\t Blue: "
                     , Serial.show blueSensorVal
                     , Serial.char '\n'
                     ]

    let newRedValue = redSensorVal / 4;
    let newGreenValue = greenSensorVal / 4;
    let newBlueValue = blueSensorVal / 4;

    Serial.device =: [ Serial.str "Mapped Sensor Values: \t Red: "
                     , Serial.show newRedValue
                     , Serial.str "\t Green: "
                     , Serial.show newGreenValue
                     , Serial.str "\t Blue: "
                     , Serial.show newBlueValue
                     , Serial.char '\n'
                     ]

    let greenLedPin = pin9
    let redLedPin = pin11
    let blueLedPin = pin10

    redLedPin =: pwm' newRedValue
    greenLedPin =: pwm' newGreenValue
    blueLedPin =: pwm' newBlueValue

pwm' :: Behavior Float -> TypedBehavior 'PWM Float
pwm' = TypedBehavior

instance (Typed a, IsPWMPin t) => Output (Pin t) (Event 'PWM (Stream a)) where
    (Pin (PinId n)) =: (Event v c) = do
        (f, triggername) <- defineTriggerAlias' ("pin_" <> show n) "analogWrite" mempty
        tell [(go triggername, const f)]
      where
        go triggername tl =
            let c' = addTriggerLimit tl c
            in trigger triggername c' [arg (constant n), arg v]

