module Miku.UI.Draw
  ( Border(..)
  , Drawable(..)
  , mkDraw
  ) where

import Brick.Types (Widget)
import Relude

import Miku.UI.State (IsMode)

data Border = Hidden
            | Rounded
            deriving stock (Eq)

class Drawable a where
  draw :: Border -> a -> Widget n


-- TODO: Name this properly
mkDraw :: (IsMode state, Drawable a) => (state -> Bool) -> (state -> a) -> state -> Widget n
mkDraw isFocus getDrawable s
  | isFocus s  = draw Rounded $ getDrawable s
  | otherwise  = draw Hidden $ getDrawable s
