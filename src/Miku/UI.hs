{-# LANGUAGE GADTs #-}
module Miku.UI
  ( drawUI
  , handleEvent
  ) where


import Brick.Types        (BrickEvent, EventM, Next, Widget)
import Brick.Widgets.Core qualified as Core
import Miku.Draw          (draw)
import Miku.Mode          (AppState (AppState), GlobalState)
import Miku.Mode          qualified as Mode
import Miku.Resource      (Res, Tick)

import Relude

drawUI :: AppState -> [Widget Res]
drawUI (AppState (s :: GlobalState emode mode)) =
  [Core.vBox [runReader (Mode.drawState @mode) s, draw $ Identity $ Mode._gsStatusLineL s]]

handleEvent :: AppState -> BrickEvent Res Tick -> EventM Res (Next AppState)
handleEvent (AppState s) event = evalStateT (Mode.handleEventState event) s
