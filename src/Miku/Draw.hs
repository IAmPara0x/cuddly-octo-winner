{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Miku.Draw
  ( Border(..)
  , Draw(..)
  , Drawable(..)
  , focusedL
  , drawableL
  , W
  ) where

import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Core          qualified as Core

import Brick.Types (Widget)
import Control.Lens (makeLenses)

import Miku.Mode (ModeState, Name)

import Relude

data Border = Hidden
            | Rounded
            deriving stock (Eq)

class Drawable a where
  draw :: a -> Widget Name

data Draw a = Draw { _focusedL  :: Bool
                   , _drawableL :: a
                   }

instance Drawable (Widget Name) where
  draw = id

instance Drawable a => Drawable (Draw a) where
  draw d
    | _focusedL d = Core.withBorderStyle Border.unicodeRounded
                  $ draw $ _drawableL d
    | otherwise   = Core.withBorderStyle (Border.borderStyleFromChar ' ')
                  $ draw $ _drawableL d
                  
  
makeLenses ''Draw

-- TODO: rename this.
type W m a = Reader (ModeState m) (Draw a)
