{-# LANGUAGE ExistentialQuantification #-}
module Miku.Draw
  ( Draw (..)
  , Drawable (..)
  , W
  , borderTypeL
  , defDraw
  , drawableL
  , focusedL
  ) where

import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Core         qualified as Core

import Brick.Types                (Widget)
import Control.Lens               (makeLenses)

import Miku.Mode                  (GlobalState, Name)

import Relude

class Drawable a where
  draw :: Draw a -> Widget Name

data Draw a
  = Draw
      { _focusedL    :: Bool
      , _borderTypeL :: Border.BorderStyle
      , _drawableL   :: a
      }

instance Drawable (Widget Name) where
  draw Draw{..} = Core.withBorderStyle _borderTypeL _drawableL

defDraw :: a -> Draw a
defDraw a = Draw { _focusedL = False, _borderTypeL = Border.borderStyleFromChar ' ', _drawableL = a}

makeLenses ''Draw

-- -- TODO: rename this.
type W emode mode a = Reader (GlobalState emode mode) (Draw a)
