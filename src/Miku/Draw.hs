{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
module Miku.Draw
  ( Draw (..)
  , Drawable (..)
  , borderTypeL
  , defDraw
  , drawableL
  , focusedL
  , whenfocused
  ) where

import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Core         qualified as Core

import Brick.Types                (Widget)
import Control.Lens               (makeLenses, (%~))

import Miku.Resource              (Res)

import Relude

class Drawable f a | a -> f where
  draw :: f a -> Widget Res

instance Drawable Draw (Widget Res) where
  draw Draw {..} = Core.withBorderStyle _borderTypeL _drawableL

data Draw a
  = Draw
      { _focusedL    :: Bool
      , _borderTypeL :: Border.BorderStyle
      , _drawableL   :: a
      }

makeLenses ''Draw

instance Functor Draw where
  fmap f = drawableL %~ f

defDraw :: a -> Draw a
defDraw a =
  Draw { _focusedL = False, _borderTypeL = Border.borderStyleFromChar ' ', _drawableL = a }

whenfocused :: (Draw a -> Draw a) -> Draw a -> Draw a
whenfocused f d | _focusedL d = f d
                | otherwise   = d

