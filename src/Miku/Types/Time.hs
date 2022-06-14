{-# OPTIONS_GHC -Wno-orphans   #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Miku.Types.Time
  ( Time,
    TimeP ,
    time,
    timeHrs,
    timeMins,
  )
where

import Miku.Types.Parser
import Relude

data Time = Time
  { timeHrs :: Integer,
    timeMins :: Integer
  }
  deriving (Show)

type TimeP = Digits <: Literal "h" <: Token ":" :>> Digits <: Literal "m"
type TimeF = Integer -> Integer -> Time

time :: TimeF
time h m = Time (h + div m 60) (mod m 60)

instance Atom TimeP where
  type AtomType TimeP = Time
  parseAtom                 = composeP @TimeP time
  showAtom (Time hrs mins)  = composeS @TimeP @TimeF "" hrs mins

instance Eq Time where
  (Time h1 m1) == (Time h2 m2) = h1 == h2 && m1 == m2

instance Ord Time where
  (Time h1 m1) <= (Time h2 m2)
    | h1 == h2 = m1 <= m2
    | otherwise = h1 <= h2

instance Num Time where
  (+) (Time h1 m1) (Time h2 m2) = time (h1 + h2) (m1 + m2)
  (*) (Time h1 m1) (Time h2 m2) = time (h1 * h2) (m1 * m2)
  abs (Time h m) = time (abs h) (abs m)
  signum (Time h m) = if h < 0 || m < 0 then Time 0 (-1) else Time 0 1
  negate (Time h m) = time (- h) (- m)
  fromInteger = time 0 . fromIntegral

instance Semigroup Time where
  (Time h1 m1) <> (Time h2 m2) = Time (h1 + h2) (m1 + m2)

instance Monoid Time where
  mempty = time 0 0
