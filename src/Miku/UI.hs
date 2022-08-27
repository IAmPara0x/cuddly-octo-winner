{-# LANGUAGE GADTs #-}

module Miku.UI (drawUI, handleEvent) where

import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  , Widget
  )
import Miku.UI.State (AppState(AppState), Name, Tick, drawState, handleEventState, IsMode)

import Relude

drawUI :: AppState -> [Widget Name]
drawUI (AppState (_ :: IsMode a => Proxy a) s) = runReader (drawState @a) s

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent (AppState _ s) event = evalStateT (handleEventState event) s
