{-# LANGUAGE ExistentialQuantification #-}
module Miku.Draw
  ( AnyWidget (..)
  , InitWidget (..)
  , W (..)
  , borderTypeL
  , changeAnyWidgetFocus
  , changeFocusL
  , drawAnyWidget
  , drawL
  , focusAnyWidget
  , focusedL
  , statusLineInfoL
  , unfocusAnyWidget
  , widgetStateL
  ) where

import Brick.Widgets.Border.Style qualified as Border

import Brick.Types                (Widget)
import Control.Lens               (makeLenses, (.~))

import Miku.Draw.StatusLine       (StatusInfo)
import Miku.Resource              (Res)

import Relude

class InitWidget a where
  initWidget :: a -> W a

data AnyWidget
  = forall a. AnyWidget (W a)

data W a
  = W
      { _focusedL        :: Bool
      , _borderTypeL     :: Border.BorderStyle
      , _widgetStateL    :: a
      , _drawL           :: W a -> Widget Res
      , _statusLineInfoL :: W a -> StatusInfo
      , _changeFocusL    :: Int -> W a -> W a
      }

makeLenses ''W

drawAnyWidget :: AnyWidget -> Widget Res
drawAnyWidget (AnyWidget w) = _drawL w w

focusAnyWidget :: AnyWidget -> AnyWidget
focusAnyWidget (AnyWidget a) =
  AnyWidget $ a & focusedL .~ True & borderTypeL .~ Border.unicodeRounded

unfocusAnyWidget :: AnyWidget -> AnyWidget
unfocusAnyWidget (AnyWidget a) =
  AnyWidget $ a & focusedL .~ False & borderTypeL .~ Border.borderStyleFromChar ' '

changeAnyWidgetFocus :: Int -> AnyWidget -> AnyWidget
changeAnyWidgetFocus n (AnyWidget w) = AnyWidget $ _changeFocusL w n w
