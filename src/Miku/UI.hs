{-# LANGUAGE GADTs #-}

module Miku.UI (drawUI, handleEvent) where

import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  , Widget
  )
import Miku.Mode (AppState(AppState), Name, Tick, drawState, handleEventState, IsMode, GlobalState)

import Relude

drawUI :: AppState -> [Widget Name]
drawUI (AppState (s :: IsMode a => GlobalState a)) = runReader (drawState @a) s

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent (AppState s) event = evalStateT (handleEventState event) s
