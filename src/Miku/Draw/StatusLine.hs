module Miku.Draw.StatusLine
  ( StatusInfo (..)
  , StatusLine (..)
  , StatusLineInfo (..)
  , drawStatusLine
  , slEditingModeL
  , slInfoL
  ) where

import Brick.Types          (Padding (Pad), Widget)
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (makeLenses)
import Data.Default         (Default (def))

import Miku.Draw            (Drawable (..))
import Miku.Editing         (EditingMode (..), SEditingMode (..))
import Miku.Resource        (Res)

import Relude

class StatusLineInfo (a :: Type) where
  statusLineInfo :: a -> [Text]

data StatusInfo
  = forall a. StatusLineInfo a => StatusInfo a

type StatusLine :: EditingMode -> Type
data StatusLine emode
  = StatusLine
      { _slEditingModeL :: SEditingMode emode
      , _slInfoL        :: [StatusInfo]
      }

instance Default (StatusLine 'Normal) where
  def = StatusLine SNormal []

makeLenses ''StatusLine

instance Drawable Identity (StatusLine a) where
  draw Identity { runIdentity = StatusLine {..} } = Core.vLimit 2 $ Core.vBox
    [ Core.vLimit 1 $ Core.hBox
      [ drawMode _slEditingModeL
      , drawInfo $ concatMap (\(StatusInfo a) -> statusLineInfo a) _slInfoL
      ]
    , Core.fill ' '
    ]

drawMode :: SEditingMode emode -> Widget Res
drawMode = Core.padLeft (Pad 1) . Core.txt . show

drawInfo :: [Text] -> Widget Res
drawInfo = Core.center . Core.hBox . (: []) . Core.txt . fold . intersperse ":"

drawStatusLine :: Text -> Text -> Widget n
drawStatusLine keys msgLog =
  Core.vBox [drawTaskBar keys, Core.vLimit 1 $ Core.padLeft (Pad 2) $ Core.txt msgLog]

drawTaskBar :: Text -> Widget n
drawTaskBar keys =
  Core.hCenter $ Border.border $ Core.vLimit 1 $ Core.hBox [cmdInfo keys, Core.fill ' ']

cmdInfo :: Text -> Widget n
cmdInfo keys = Core.padLeft (Pad 2) $ Core.txt $ "CMD: " <> keys
