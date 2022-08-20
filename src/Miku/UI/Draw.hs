module Miku.UI.Draw
  ( Border(..)
  , Drawable(..)
  , drawWidget
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

drawWidget :: (IsMode state, Drawable a) => Reader state Bool -> Reader state a -> Reader state (Widget n)
drawWidget isFocus rdraw = do
  s <- ask
  if runReader isFocus s
    then return $ draw Rounded $ runReader rdraw s
    else return $ draw Hidden $ runReader rdraw s

