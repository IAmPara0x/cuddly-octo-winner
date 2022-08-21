module Miku.UI.Draw
  ( Border(..)
  , Draw
  , Drawable(..)
  , drawWidget
  ) where

import Brick.Types (Widget)
import Relude

import Miku.UI.State (IsMode, ModeState, Name)

data Border = Hidden
            | Rounded
            deriving stock (Eq)

class Drawable a where
  draw :: Border -> a -> Widget n


-- type DrawX mode a = (IsMode mode, Drawable a) => Reader mode a
type Draw a = Reader (ModeState a) (Widget Name)

drawWidget :: (IsMode mode, Drawable a)
           => Reader (ModeState mode) Bool
           -> Reader (ModeState mode) a
           -> Draw mode
drawWidget isFocus rdraw = do
  focus <- isFocus
  widget <- rdraw
  if focus
    then return $ draw Rounded widget
    else return $ draw Hidden widget

