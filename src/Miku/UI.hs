{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Miku.UI (drawUI, handleEvent) where

import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  , Widget
  )
import Miku.UI.State (AppState(AppState), Name, Tick, drawState, handleEventState)

drawUI :: AppState -> [Widget n]
drawUI (AppState s) = drawState s

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent (AppState s) = handleEventState s
