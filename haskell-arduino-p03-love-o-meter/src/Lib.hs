module Lib where

import Copilot.Arduino.Uno
import qualified Copilot.Arduino.Library.Serial as Serial


someFunc = arduino $ do
    sensorVal <- input a0 :: Sketch (Behavior ADC)

    let baselineTemp = constant 20.0 :: Behavior Float
    let sensorValFloat = unsafeCast sensorVal :: Behavior Float
    let voltage = (sensorValFloat / constant 1024) * constant 5
    let temperature = (voltage - constant 0.5) * constant 100

    Serial.baud 9600
    Serial.device =: [ Serial.str "Sensor Value: "
                     , Serial.show sensorVal
                     , Serial.char '\n'
                     , Serial.str "Volts: "
                     , Serial.show voltage
                     , Serial.char '\n'
                     , Serial.str "Degrees C: "
                     , Serial.show temperature
                     , Serial.char '\n'
                     ]

    if temperature < baselineTemp then do
        pin2 =: false
        pin3 =: false
        pin4 =: false
    else if temperature >= (baselineTemp + constant 2) && temperature < (baselineTemp + constant 4) then do
        pin2 =: true
        pin3 =: false
        pin4 =: false
    else if temperature >= (baselineTemp + constant 4) && temperature < (baselineTemp + constant 6) then do
        pin2 =: true
        pin3 =: true
        pin4 =: false
    else do
        pin2 =: true
        pin3 =: true
        pin4 =: true

    delay =: MilliSeconds (constant 300)

