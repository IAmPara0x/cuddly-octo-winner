module Miku.UI.Events.Welcome (handleWelcomeStateEvent) where

import Brick.Main (continue, halt)

import Brick.Types
  ( BrickEvent(VtyEvent)
  , EventM
  , Next
  )

import Control.Lens ((^.), (<>~))
import Data.Map qualified as Map

import Graphics.Vty qualified as Vty

import Miku.UI.State (AppState(WState), Name, Tick)
import Miku.UI.State.Welcome (WelcomeState, wsActionsL, wsPrevKeysL)

import Relude


execute :: WelcomeState -> EventM Name (Next AppState)
execute wstate = 
  case Map.lookup (wstate ^. wsPrevKeysL) (wstate ^. wsActionsL) of
    Just action -> continue $ WState (action wstate)
    Nothing     -> continue $ WState wstate
  

handleWelcomeStateEvent :: WelcomeState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleWelcomeStateEvent wstate (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) =
  halt $ WState wstate
handleWelcomeStateEvent wstate (VtyEvent (Vty.EvKey (Vty.KChar c) [])) =
  execute (wstate & wsPrevKeysL <>~ [c])
handleWelcomeStateEvent wstate _ =
  continue $ WState wstate
