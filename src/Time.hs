module Time ( Time()
            , newTime
            , readTime
            ) where

import Data.Bifunctor (bimap)

mins :: Int
mins = 60

hrs :: Int
hrs = 24

data Time = Time Int Int

newTime :: Int -> Int -> Time
newTime h m = Time hnew mnew
  where
    mnew = mod m mins
    hnew = h + div m mins

instance Show Time where
  show (Time h m) = concat [show h, "H:", show m, "M"]

-- NOTE: readTime doesn't Handle errors.
readTime :: String -> Time
readTime = (\(h,m) -> newTime (read h::Int) (read m::Int)) . bimap init (init . tail) . span (/= ':')

instance Eq Time where
  (==) (Time h1 m1) (Time h2 m2) = h1 == h2 && m1 == m2

instance Ord Time where
  (<=) (Time h1 m1) (Time h2 m2) = h1 <= h2 && m1 <= m2

instance Num Time where
  (+) (Time h1 m1) (Time h2 m2) = newTime (h1 + h2) (m1 + m2)
  (*) (Time h1 m1) (Time h2 m2) = newTime (h1 * h2) (m1 * m2)
  abs (Time h1 m1)              = newTime (abs h1) (abs m1)
  signum (Time h1 m1)           = error "Error: Time does not have `negate` function implemented."
  negate (Time h1 m1)           = newTime (- h1) (- m1)
  fromInteger                   = newTime 0 . fromIntegral

