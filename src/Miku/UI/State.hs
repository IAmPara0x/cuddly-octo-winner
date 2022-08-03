{-# LANGUAGE GADTs #-}
module Miku.UI.State
  ( AppState(WState)
  , Name
  , Tick
  )
  where

import Miku.UI.State.Welcome (WelcomeState)

data AppState where
  WState :: WelcomeState -> AppState

type Name = ()
type Tick = ()
