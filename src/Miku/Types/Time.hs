module Miku.Types.Time ( Time
                       , time
                       , timeP
                       ) where

import           Data.Char (isNumber)
import           Text.Read (read)
import qualified Data.Text as Text

import Relude

import Miku.Types.Parser

data Time = Time { timeHrs :: Int
                 , timeMins :: Int
                 }
            deriving (Show)

instance Eq Time where
  (==) (Time h1 m1) (Time h2 m2) = h1 == h2 && m1 == m2

instance Ord Time where
  (<=) (Time h1 m1) (Time h2 m2) = h1 <= h2 && m1 <= m2

instance Num Time where
  (+) (Time h1 m1) (Time h2 m2) = time (h1 + h2) (m1 + m2)
  (*) (Time h1 m1) (Time h2 m2) = time (h1 * h2) (m1 * m2)
  abs (Time h1 m1)              = time (abs h1) (abs m1)
  signum (Time h1 m1)           = error "Error: Time does not have `signum` function implemented."
  negate (Time h1 m1)           = time (- h1) (- m1)
  fromInteger                   = time 0 . fromIntegral

time :: Int -> Int -> Time
time h m = Time (h + div m 60) (mod m 60)
    

timeP :: Parser Time
timeP = do
  hrs   <- spaceP *> takeWhileP (Just "valid hour") isNumber
  void (char 'H')
  void (tokenP $ char ':')
  
  mins  <- spaceP *> takeWhileP (Just "valid mins") isNumber
  void (char 'M')
  return $ time (read $ Text.unpack hrs) (read $ Text.unpack mins)
