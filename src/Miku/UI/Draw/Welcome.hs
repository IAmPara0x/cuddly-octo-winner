module Miku.UI.Draw.Welcome (drawWelcomeState) where


import Brick.Types ( Widget )
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens ((^.))

import Miku.UI.Draw.StatusLine (drawStatusLine)
import Miku.UI.State (WelcomeState, wsMsgL, wsPrevKeysL)

import Data.Text qualified as Text

import Relude

drawWelcomeState :: WelcomeState -> [Widget n]
drawWelcomeState wstate =
  [Core.vBox
    [ Core.vLimitPercent 95 $ Core.center $ Core.txt (wstate ^. wsMsgL)
    , drawStatusLine ("CMD: " <> Text.pack (wstate ^. wsPrevKeysL))
    ]
  ]
