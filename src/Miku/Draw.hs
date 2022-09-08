{-# LANGUAGE ExistentialQuantification #-}
module Miku.Draw
  ( Draw(..)
  , Drawable(..)
  , focusedL
  , drawableL
  , borderTypeL
  , defDraw
  , W
  ) where

import qualified Brick.Widgets.Border.Style as Border
import qualified Brick.Widgets.Core         as Core

import           Brick.Types                (Widget)
import           Control.Lens               (makeLenses)

import           Miku.Mode                  (GlobalState, Name)

import           Relude

class Drawable a where
  draw :: Draw a -> Widget Name

data Draw a = Draw { _focusedL    :: Bool
                   , _borderTypeL :: Border.BorderStyle
                   , _drawableL   :: a
                   }

instance Drawable (Widget Name) where
  draw Draw{..} = Core.withBorderStyle _borderTypeL _drawableL

defDraw :: a -> Draw a
defDraw a = Draw { _focusedL = False, _borderTypeL = Border.borderStyleFromChar ' ', _drawableL = a}

makeLenses ''Draw

-- -- TODO: rename this.
type W m a = Reader (GlobalState m) (Draw a)
