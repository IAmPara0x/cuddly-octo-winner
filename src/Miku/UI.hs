{-# LANGUAGE GADTs #-}

module Miku.UI (drawUI, handleEvent) where

import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  , Widget
  )
import Miku.UI.State (AppState(WState), Name, Tick)
import Miku.UI.Draw.Welcome (drawWelcomeState)
import Miku.UI.Events.Welcome (handleWelcomeStateEvent)

drawUI :: AppState -> [Widget n]
drawUI (WState wstate) = drawWelcomeState wstate

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent (WState wstate) = handleWelcomeStateEvent wstate
