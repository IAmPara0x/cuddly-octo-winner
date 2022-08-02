{-# LANGUAGE GADTs #-}
module Miku.UI.State
  ( AppState(WState)
  , Name
  , Tick
  )
  where

import Miku.UI.State.Welcome (WelcomeConfig, WelcomeState)

data AppState where
  WState :: WelcomeConfig -> WelcomeState -> AppState

type Name = ()
type Tick = ()
