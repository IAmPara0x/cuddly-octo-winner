{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Miku.Draw
  ( Border(..)
  , Draw(..)
  , Drawable(..)
  , focusedL
  , drawableL
  , borderTypeL
  , W
  ) where

import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Core          qualified as Core

import Brick.Types (Widget)
import Control.Lens (makeLenses)

import Miku.Mode (GlobalState, Name)

import Relude

data Border = Hidden
            | Rounded
            deriving stock (Eq)

class Drawable a where
  draw :: a -> Widget Name

data Draw a = Draw { _focusedL    :: Bool
                   , _borderTypeL :: Border.BorderStyle
                   , _drawableL   :: a
                   }

instance Drawable (Widget Name) where
  draw = id

instance Drawable a => Drawable (Draw a) where
  draw d = Core.withBorderStyle (_borderTypeL d)
         $ draw $ _drawableL d


makeLenses ''Draw

-- TODO: rename this.
type W m a = Reader (GlobalState m) (Draw a)
