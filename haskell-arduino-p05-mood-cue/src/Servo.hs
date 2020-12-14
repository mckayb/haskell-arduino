{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Servo where

import Data.Function (const)
import Control.Monad.Writer (MonadWriter(tell))
import Copilot.Arduino.Internals
import Copilot.Arduino.Uno

data Servo a = Servo { minPulseWidth :: Int, maxPulseWidth :: Int, pin :: a }

defaultPulseWidth :: Integer
defaultPulseWidth = 1500

servo :: IsPWMPin p => Pin p -> Servo (Pin p)
servo = Servo minPulseWidth maxPulseWidth
  where
    minPulseWidth = 544
    maxPulseWidth = 2400

{-
transform' :: Fractional a => a -> a -> a -> a -> a -> a
transform' lmin lmax rmin rmax value = 
  (((value - lmin) * (rmax - rmin)) / (lmax - lmin)) + rmin

transform :: Fractional a => a -> a
transform = transform' 0 1023 0 179
-}

setAngle :: IsPWMPin p => Servo (Pin p) -> Int -> Sketch ()
setAngle (Servo minPulse maxPulse pin) angle = do
  let floatMin = fromIntegral minPulse
  let floatMax = fromIntegral maxPulse
  let floatAngle = fromIntegral angle

  let duration = floatMin + (((floatMax - floatMin) * floatAngle) / 180) :: Float
  pin =: pwm' (constF duration)


-- The library only defines pwm for Word8, so we did some hacking here to allow floats.
pwm' :: Behavior a -> TypedBehavior 'PWM a
pwm' = TypedBehavior

instance (Typed a, IsPWMPin t) => Output (Pin t) (Event 'PWM (Stream a)) where
    (Pin (PinId n)) =: (Event v c) = do
        (f, triggername) <- defineTriggerAlias' ("pin_" <> show n) "analogWrite" mempty
        tell [(go triggername, const f)]
      where
        go triggername tl =
            let c' = addTriggerLimit tl c
            in trigger triggername c' [arg (constant n), arg v]





{-
150 / 250 ->        
-}

{-
Servo's move by Pulse Width Modulation, or PWM.
The usual frequency of these PWM signals is 50Hz, which is every 20 milliseconds.
The servomotor has a rotating shaft and potentiometer that detects it's position.
When there is a pulse from the control signal, it applies current to the motor that
makes the shaft move until the potentiometer indicates that the position is
in line with the width of the pulse.

A servomotor turns 90 degrees in either direction. Maximum movement is 180.
Width of the pulses determine the position of the shaft. For eample, a pulse of 1ms
will move the shaft anticlockwise at -90 degress. A pulse of 1.5ms will move the shaft
at the neutral position at 0 degrees and a pulse of 2 ms will move the shaft at +90 degrees.
-}

{-
Following code stolen from here for guidance as I try to make one that works with Copilot
http://hackage.haskell.org/package/hArduino-1.1/docs/src/System-Hardware-Arduino-Parts-Servo.html#Servo
-- | A servo motor. Note that this type is abstract, use 'attach' to
-- create an instance.
data Servo = Servo { servoPin :: IPin  -- ^ The internal-pin that controls the servo
                   , minPulse :: Int   -- ^ Pulse-width (microseconds) for the minumum (0-degree) angle.
                   , maxPulse :: Int   -- ^ Pulse-width (microseconds) for the maximum (typically 180-degree) angle.
                   }

-- | Create a servo motor instance. The default values for the min/max angle pulse-widths, while typical,
-- may need to be adjusted based on the specs of the actual servo motor. Check the data-sheet for your
-- servo to find the proper values. The default values of @544@ and @2400@ microseconds are typical, so you might
-- want to start by passing 'Nothing' for both parameters and adjusting as necessary.
attach :: Pin            -- ^ Pin controlling the servo. Should be a pin that supports SERVO mode.
       -> Maybe Int      -- ^ Pulse-width (in microseconds) for the minumum 0-degree angle. Default: @544@.
       -> Maybe Int      -- ^ Pulse-width (in microseconds) for the maximum, typically 180-degree, angle. Default: @2400@.
       -> Arduino Servo
attach p mbMin mbMax
  | Just m <- mbMin, m < 0
  = die "Servo.attach: minimum pulse width must be positive" ["Received: " ++ show m]
  | Just m <- mbMax, m < 0
  = die "Servo.attach: maximum pulse width must be positive" ["Received: " ++ show m]
  | True
  = do let minPulse = fromMaybe 544  mbMin
           maxPulse = fromMaybe 2400 mbMax
       debug $ "Attaching servo on pin: " ++ show p ++ " with parameters: " ++ show (minPulse, maxPulse)
       when (minPulse >= maxPulse) $ die "Servo.attach: min pulse duration must be less than max pulse duration"
                                         [ "Received min-pulse: " ++ show minPulse
                                         , "Received max-pulse: " ++ show maxPulse
                                         ]
       setPinMode p SERVO
       (ip, _) <- convertAndCheckPin "Servo.attach" p SERVO
       return Servo { servoPin = ip
                    , minPulse = fromMaybe 544  mbMin
                    , maxPulse = fromMaybe 2400 mbMax
                    }

-- | Set the angle of the servo. The argument should be a number between 0 and 180,
-- indicating the desired angle setting in degrees.
setAngle :: Servo -> Int -> Arduino ()
setAngle Servo{servoPin, minPulse, maxPulse} angle
  | angle < 0 || angle > 180
  = die "Servo.setAngle: angle must be between 0 and 180." ["Received: " ++ show angle]
  | True
  = do let duration = minPulse + ((maxPulse - minPulse) * angle) `div` 180
       debug $ "Setting servo on pin: " ++ show servoPin ++ " " ++ show angle ++ " degrees, via a pulse of " ++ show duration ++ " microseconds."
       -- In arduino, the most we can send is 16383; not that a servo should need such a large value, but
       -- just in case
       when (duration >= 16383) $ die "Servo.setAngle angle setting: out-of-range."
                                      [ "Servo pin         : " ++ show servoPin
                                      , "Angle required    : " ++ show angle
                                      , "Min pulse duration: " ++ show minPulse
                                      , "Max pulse duration: " ++ show maxPulse
                                      , "Duration needed   : " ++ show duration
                                      , "Exceeds max value : 16383"
                                      ]
       let msb = fromIntegral $ (duration `shiftR` 7) .&. 0x7f
           lsb = fromIntegral $ duration .&. 0x7f
       send $ AnalogPinWrite servoPin lsb msb

-}