module Miku.Types.Time
  ( Time
  , getCurrentDay
  , getCurrentTime
  , mkTime
  , showTime
  , timeHrsL
  , timeMinsL
  ) where

import Control.Lens      (makeLenses)
import Data.Time         (Day, getZonedTime, localDay, localTimeOfDay, todHour,
                          todMin, zonedTimeToLocalTime)

import Miku.Types.Parser

import Relude


data Time
  = Time
      { _timeHrsL  :: Integer
      , _timeMinsL :: Integer
      }
  deriving stock (Show)

type TimeFormat = Digits <: Literal "h" <: Token ":" :+> Digits <: Literal "m"
type TimeF      = Integer -> Integer -> Time

instance MkBluePrint Time where
  type Format Time   = TimeFormat
  type Function Time = TimeF

  parseBP = mkTime
  showBP (Time hrs mins)  = composeS @TimeFormat @TimeF "" hrs mins

-- parseTime :: Parser
-- parseTime

showTime :: Time -> Text
showTime = showAtom @(BluePrint Time)

mkTime :: TimeF
mkTime h m = Time (h + div m 60) (mod m 60)

instance Eq Time where
  (Time h1 m1) == (Time h2 m2) = h1 == h2 && m1 == m2

instance Ord Time where
  (Time h1 m1) <= (Time h2 m2)
    | h1 == h2  = m1 <= m2
    | otherwise = h1 <= h2

-- TODO: Test time arithmetic
instance Num Time where
  (+) (Time h1 m1) (Time h2 m2) = mkTime (h1 + h2) (m1 + m2)
  (*) (Time h1 m1) (Time h2 m2) = mkTime (h1 * h2) (m1 * m2)
  abs (Time h m)                = mkTime (abs h) (abs m)
  signum (Time h m)             = if h < 0 || m < 0 then Time 0 (-1) else Time 0 1
  negate (Time h m)             = mkTime (- h) (- m)
  fromInteger                   = mkTime 0

instance Semigroup Time where
  (Time h1 m1) <> (Time h2 m2) = Time (h1 + h2) (m1 + m2)

instance Monoid Time where
  mempty = mkTime 0 0

makeLenses ''Time

getCurrentTime :: IO Time
getCurrentTime = (\t -> mkTime (toInteger $ todHour t) (toInteger $ todMin t))
               . localTimeOfDay
               . zonedTimeToLocalTime
               <$> getZonedTime

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime
