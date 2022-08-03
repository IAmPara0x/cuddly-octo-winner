{-# LANGUAGE ViewPatterns #-}

module Miku.UI.Events.Welcome (handleWelcomeStateEvent) where

import Brick.Main (continue, halt)

import Brick.Types
  ( BrickEvent(VtyEvent)
  , EventM
  , Next
  )

import Control.Lens ((^.), (<>~), (.~))
import Data.Map qualified as Map

import Graphics.Vty (Key(KChar, KEsc))
import Graphics.Vty qualified as Vty

import Miku.UI.State
  ( AppState(WState)
  , Name
  , Tick
  , WelcomeState
  , wsActionsL
  , wsPrevKeysL
  )

import Relude

handleWelcomeStateEvent :: WelcomeState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleWelcomeStateEvent wstate (eventKey -> Just (KChar 'q'))
  = halt $ WState wstate
handleWelcomeStateEvent wstate (eventKey -> Just KEsc)
  = execute (wstate & wsPrevKeysL .~ [])
handleWelcomeStateEvent wstate (eventKey -> Just (KChar c))
  = execute (wstate & wsPrevKeysL <>~ [c])
handleWelcomeStateEvent wstate  _
  = continue $ WState wstate

execute :: WelcomeState -> EventM Name (Next AppState)
execute wstate = 
  case Map.lookup (wstate ^. wsPrevKeysL) (wstate ^. wsActionsL) of
    Just action -> action wstate
    Nothing     -> continue $ WState wstate

eventKey :: BrickEvent Name Tick -> Maybe Vty.Key
eventKey (VtyEvent (Vty.EvKey key _)) = Just key
eventKey _                            = Nothing
