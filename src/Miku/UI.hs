{-# LANGUAGE GADTs #-}
module Miku.UI
  ( drawUI
  , handleEvent
  ) where


import Brick.Types        (BrickEvent, EventM, Next, Widget)
import Brick.Widgets.Core qualified as Core
import Miku.Draw          (draw)
import Miku.Mode
  ( AppState (AppState)
  , GlobalState
  , Tick
  , _gsStatusLineL
  , drawState
  , handleEventState
  )
import Miku.Resource      (Res)

import Relude

drawUI :: AppState -> [Widget Res]
drawUI (AppState (s :: GlobalState emode mode)) =
  [Core.vBox [runReader (drawState @mode) s, draw $ Identity $ _gsStatusLineL s]]

handleEvent :: AppState -> BrickEvent Res Tick -> EventM Res (Next AppState)
handleEvent (AppState s) event = evalStateT (handleEventState event) s
