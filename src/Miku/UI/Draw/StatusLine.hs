module Miku.UI.Draw.StatusLine (drawStatusLine) where

import Brick.Types (Padding (Pad), Widget)
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Relude


drawStatusLine :: Text -> Widget n
drawStatusLine currentState =
      Core.hCenter
    $ Core.withBorderStyle Border.unicodeRounded
    $ Border.borderWithLabel (Core.txt "Status")
    $ Core.hBox [ Core.padLeft (Pad 2) $ Core.txt currentState
                , Core.fill ' '
                ]

