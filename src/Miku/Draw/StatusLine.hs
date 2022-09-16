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

import Miku.Draw            (Draw (..), Drawable (..))
import Miku.Editing         (EditingMode (..), SEditingMode (..))
import Miku.Mode            (Name)

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

makeLenses ''StatusLine

instance Drawable (StatusLine a) where
  draw Draw { _drawableL = StatusLine {..} } = Core.vLimit 2 $ Core.vBox
    [ Core.vLimit 1 $ Core.hBox
      [ drawMode _slEditingModeL
      , drawInfo $ concatMap (\(StatusInfo a) -> statusLineInfo a) _slInfoL
      ]
    , Core.fill ' '
    ]

drawMode :: SEditingMode emode -> Widget Name
drawMode = Core.padLeft (Pad 1) . Core.txt . show

drawInfo :: [Text] -> Widget Name
drawInfo = Core.center . Core.hBox . (: []) . Core.txt . fold . intersperse ":"

drawStatusLine :: Text -> Text -> Widget n
drawStatusLine keys msgLog = Core.vBox
  [drawTaskBar keys, Core.vLimit 1 $ Core.padLeft (Pad 2) $ Core.txt msgLog]

drawTaskBar :: Text -> Widget n
drawTaskBar keys = Core.hCenter $ Border.border $ Core.vLimit 1 $ Core.hBox
  [cmdInfo keys, Core.fill ' ']

cmdInfo :: Text -> Widget n
cmdInfo keys = Core.padLeft (Pad 2) $ Core.txt $ "CMD: " <> keys
