{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Servo where

import Data.Function (const)
import Control.Monad.Writer (MonadWriter(tell))
import Copilot.Arduino.Internals
import Copilot.Arduino.Uno

transform' :: Fractional a => a -> a -> a -> a -> a -> a
transform' lmin lmax rmin rmax value = 
  (((value - lmin) * (rmax - rmin)) / (lmax - lmin)) + rmin

transform :: Fractional a => a -> a
transform = transform' 0 1023 0 179

-- The library only defines pwm for Word8, so we did some hacking here to allow floats.
pwm' :: Behavior a -> TypedBehavior 'PWM a
pwm' = TypedBehavior

instance (IsPWMPin t) => Output (Pin t) (Event 'PWM (Stream Float)) where
    (Pin (PinId n)) =: (Event v c) = do
        (f, triggername) <- defineTriggerAlias' ("pin_" <> show n) "analogWrite" mempty
        tell [(go triggername, const f)]
      where
        go triggername tl =
            let c' = addTriggerLimit tl c
            in trigger triggername c' [arg (constant n), arg v]