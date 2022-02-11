{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Time ( Time
                      , newTime
                      , timeP
                      )
                      where

import Relude
import qualified Data.Text as T

import Types
import Parser

mins :: Int
mins = 60

hrs :: Int
hrs = 24

data Time = Time Int Int
            deriving (Show)

newTime :: Int -> Int -> Time
newTime h m = Time hnew mnew
  where
    mnew = mod m mins
    hnew = h + div m mins

-- Syntax for Time
timeHrs :: Char
timeHrs = 'H'

timeSep :: Char
timeSep = ':'

timeMins :: Char
timeMins = 'M'
--

instance Put Time where
  put (Time h m) = T.concat [T.pack $ show h,
                             T.singleton timeHrs,
                             T.singleton timeSep,
                             T.pack $ show m,
                             T.singleton timeMins
                             ]

instance Eq Time where
  (==) (Time h1 m1) (Time h2 m2) = h1 == h2 && m1 == m2

instance Ord Time where
  (<=) (Time h1 m1) (Time h2 m2) = h1 <= h2 && m1 <= m2

instance Num Time where
  (+) (Time h1 m1) (Time h2 m2) = newTime (h1 + h2) (m1 + m2)
  (*) (Time h1 m1) (Time h2 m2) = newTime (h1 * h2) (m1 * m2)
  abs (Time h1 m1)              = newTime (abs h1) (abs m1)
  signum (Time h1 m1)           = error "Error: Time does not have `signum` function implemented."
  negate (Time h1 m1)           = newTime (- h1) (- m1)
  fromInteger                   = newTime 0 . fromIntegral


-- Parser Implementation for Time

timeP :: Parser Time
timeP = newTime <$> tokenP intP <* symbCharP timeHrs <* symbCharP timeSep <*>
                    tokenP intP <* symbCharP 'M'
