module Lib
    ( someFunc
    ) where

import Copilot.Arduino

someFunc = arduino $ do
    led =: blinking
    delay =: MilliSeconds (constant 100)
