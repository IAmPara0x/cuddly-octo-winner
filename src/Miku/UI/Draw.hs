module Miku.UI.Draw
  ( Border(..)
  , Drawable(..)
  ) where

import Brick.Types (Widget)
import Relude

data Border = Hidden
            | Rounded
            deriving stock (Eq)

class Drawable a where
  draw :: Border -> a -> Widget n
