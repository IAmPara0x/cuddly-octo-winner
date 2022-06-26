{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Miku.UI
  ( Layout(Main)
  , MainState(MainState)
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

data MainState = MainState { _msCurrentLogL  :: Log
                           , _msCurrentTimeL :: Time
                           } deriving stock (Show)

makeLenses ''MainState

data UI (mode :: Layout) where
  MainUI :: MainState -> UI 'Main
  
data Tick = Tick
            deriving stock (Show)
