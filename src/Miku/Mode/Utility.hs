module Miku.Mode.Utility
  ( Window
  , horizMove
  , vertMove
  , viewWindow
  , window
  ) where


import GHC.Natural (naturalToInt)
import Relude

data Window (x :: Nat) (y :: Nat)
  = Window Int Int

viewWindow :: Window x y -> (Int, Int)
viewWindow (Window x y) = (x,y)

window :: forall x y. (KnownNat x, KnownNat y) => Int -> Int -> Window x y
window x1 y1 = Window (clampToZero $ min x1 xmax) (clampToZero $ min y1 ymax)
  where xmax = naturalToInt $ natVal @x Proxy - 1
        ymax = naturalToInt $ natVal @y Proxy - 1

horizMove :: forall x y. (KnownNat x) => Int -> Window x y -> Window x y
horizMove x2 (Window x1 y1) = Window (clampToZero $ min xmax $ x1 + x2) y1
  where xmax = naturalToInt $ natVal @x Proxy - 1

vertMove :: forall x y. (KnownNat y) => Int -> Window x y -> Window x y
vertMove y2 (Window x1 y1) = Window x1 (clampToZero $ min ymax $ y1 + y2)
  where ymax = naturalToInt $ natVal @y Proxy - 1

clampToZero :: Int -> Int
clampToZero x
  | x < 0     = 0
  | otherwise = x

