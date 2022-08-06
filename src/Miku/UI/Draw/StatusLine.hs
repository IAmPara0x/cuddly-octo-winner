module Miku.UI.Draw.StatusLine (drawStatusLine) where

import Brick.Types (Padding (Pad), Widget)
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Relude


drawStatusLine :: Text -> Text -> Widget n
drawStatusLine keys msgLog =
  Core.vBox [ drawTaskBar keys
            , Core.vLimit 1 $ Core.padLeft (Pad 2) $ Core.txt msgLog
            ]

drawTaskBar :: Text -> Widget n
drawTaskBar keys =
      Core.hCenter
    $ Core.withBorderStyle Border.unicodeRounded
    $ Border.border
    $ Core.vLimit 1
    $ Core.hBox [ cmdInfo keys
                , Core.fill ' '
                ]

cmdInfo :: Text -> Widget n
cmdInfo keys = Core.padLeft (Pad 2) $ Core.txt $ "CMD: " <> keys
