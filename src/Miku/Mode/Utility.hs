module Miku.Mode.Utility
  ( FocusRing2D (_currFocusL, _allDataL)
  , focusRing2D
  , getFocused
  , horizMove
  , modifyFocused
  , putFocused
  , vertMove
  ) where


import Control.Lens (ix, makeLenses, (%~))
import Data.Map     qualified as Map
import GHC.Natural  (naturalToInt)
import Relude

data FocusRing2D (x :: Nat) (y :: Nat) a
  = FocusRing2D
      { _currFocusL :: (Int, Int)
      , _allDataL   :: Map (Int, Int) a
      }
makeLenses ''FocusRing2D

getFocused :: FocusRing2D x y a -> Either Text a
getFocused FocusRing2D{..} = maybeToRight ("Following key [ " <> show _currFocusL <> " ] was not present in the map") (Map.lookup _currFocusL _allDataL)

modifyFocused :: (a -> a) -> FocusRing2D x y a -> FocusRing2D x y a
modifyFocused f d = d & allDataL . ix (_currFocusL d) %~ f

putFocused :: a -> FocusRing2D x y a -> FocusRing2D x y a
putFocused a = modifyFocused (const a)

focusRing2D :: forall x y a. (KnownNat x, KnownNat y) => (Int,Int) -> Map (Int, Int) a -> Either Text (FocusRing2D x y a)
focusRing2D (x1,y1) alldata
  | allKeys == possibleKeys = Right $ FocusRing2D (clampToZero $ min x1 xmax, clampToZero $ min y1 ymax) alldata
  | otherwise               = Left "Not all possible keys where present in the provided map."
  where

    xmax,ymax :: Int
    xmax = naturalToInt $ natVal @x Proxy - 1
    ymax = naturalToInt $ natVal @y Proxy - 1

    allKeys,possibleKeys :: Set (Int,Int)
    allKeys = Map.keysSet alldata
    possibleKeys = fromList $ [(x,y) | x <- [0..xmax], y <- [0..ymax]]

horizMove :: forall x y a. (KnownNat x) => Int -> FocusRing2D x y a -> FocusRing2D x y a
horizMove x2 f = f & currFocusL %~ (\(x1,y1) -> (clampToZero $ min xmax (x1 + x2), y1))
  where xmax = naturalToInt $ natVal @x Proxy - 1

vertMove :: forall x y a. (KnownNat y) => Int -> FocusRing2D x y a -> FocusRing2D x y a
vertMove y2 f = f & currFocusL %~ (\(x1,y1) -> (x1, clampToZero $ min ymax (y1 + y2)))
  where ymax = naturalToInt $ natVal @y Proxy - 1

clampToZero :: Int -> Int
clampToZero x
  | x < 0     = 0
  | otherwise = x
