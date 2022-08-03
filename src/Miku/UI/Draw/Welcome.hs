module Miku.UI.Draw.Welcome (drawWelcomeState) where


import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core (txt)
import Brick.Widgets.Center qualified as C

import Control.Lens ((^.))

import Miku.UI.State.Welcome (WelcomeState, wsMsgL)

import Relude

drawWelcomeState :: WelcomeState -> [Widget n]
drawWelcomeState wstate =
  [C.center $ txt (wstate ^. wsMsgL)
  ]
