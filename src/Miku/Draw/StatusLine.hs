module Miku.Draw.StatusLine
  ( StatusInfo (..)
  , StatusLine (..)
  , drawStatusLine
  , slEditingModeL
  , slInfoL
  ) where

import Brick.Types          (Padding (Pad), Widget)
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (makeLenses)
import Data.Default         (Default (def))
import Data.Text            qualified as Text

import Miku.Editing         (EditingMode (..), SEditingMode (..))
import Miku.Resource        (Res)

import Relude

newtype StatusInfo
  = StatusInfo { _getStatusLineInfoL :: [Text] }
  deriving newtype (Monoid, Semigroup)

instance IsString StatusInfo where
  fromString s = StatusInfo [Text.pack s]

type StatusLine :: EditingMode -> Type
data StatusLine emode
  = StatusLine
      { _slEditingModeL :: SEditingMode emode
      , _slInfoL        :: [StatusInfo]
      }

instance Default (StatusLine 'Normal) where
  def = StatusLine SNormal []

makeLenses ''StatusLine

drawStatusLine :: StatusLine e -> Widget Res
drawStatusLine StatusLine {..} = Core.vLimit 2 $ Core.vBox
  [ Core.vLimit 1 $ Core.hBox [drawMode _slEditingModeL, drawInfo $ concatMap coerce _slInfoL]
  , Core.fill ' '
  ]

drawMode :: SEditingMode emode -> Widget Res
drawMode = Core.padLeft (Pad 1) . Core.txt . show

drawInfo :: [Text] -> Widget Res
drawInfo = Core.center . Core.hBox . (: []) . Core.txt . fold . intersperse ":"
