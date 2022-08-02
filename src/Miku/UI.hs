{-# LANGUAGE GADTs #-}

module Miku.UI (drawUI, handleEvent) where

import Brick.Main (continue, halt)

import Brick.Types
  ( BrickEvent(VtyEvent)
  , EventM
  , Next
  , Widget
  )

import Graphics.Vty qualified as Vty

import Miku.UI.State (AppState(WState), Name, Tick)
import Miku.UI.State.Welcome (drawWelcomeState)

import Relude ()

  
drawUI :: AppState -> [Widget n]
drawUI (WState wconfig wstate)= drawWelcomeState wconfig wstate

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent ui (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt ui
handleEvent ui _                                         = continue ui
