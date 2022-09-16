{-# LANGUAGE GADTs #-}
module Miku.UI
  ( drawUI
  , handleEvent
  ) where

import Brick.Types (BrickEvent, EventM, Next, Widget)
import Miku.Mode
  ( AppState (AppState)
  , GlobalState
  , IsMode
  , Name
  , Tick
  , drawState
  , handleEventState
  )

import Relude

drawUI :: AppState -> [Widget Name]
drawUI (AppState (s :: IsMode mode => GlobalState emode mode)) = runReader drawState s

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent (AppState s) event = evalStateT (handleEventState event) s
