{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Miku.UI
  ( Layout(Main)
  , MainState(MainState)
  , msClockAnimationStateL
  , msCurrentLogL
  , msCurrentTimeL
  , ResourceName
  , Tick(Tick)
  , UI(MainUI)
  )
  where


import Control.Lens (makeLenses)
import Relude

import Miku.Types.Log

data Layout  = Main
             deriving stock (Show)

type ResourceName = ()

data MainState = MainState { _msCurrentLogL          :: Log
                           , _msCurrentTimeL         :: Time
                           , _msClockAnimationStateL :: Integer
                           } deriving stock (Show)

makeLenses ''MainState

data UI (mode :: Layout) where
  MainUI :: MainState -> UI 'Main

data Tick = Tick
            deriving stock (Show)
