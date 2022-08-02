{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}

module Miku.UI
  ( Layout(Main)
  , MainState(MainState)
  , msClockAnimationStateL
  , msCurrentLogL
  , msCurrentTimeL
  , msCurrentWindowL
  , MainRes(..)
  , Resource(MainRes)
  , Tick(Tick)
  , UI(MainUI)
  )
  where


import Text.Show qualified
import Control.Lens (makeLenses)
import Relude

import Miku.Types.Log

data Layout  = Main
             deriving stock (Show, Eq, Ord)

data MainRes = CurrentTask
             | NotCompletedGoals
             | CompletedGoals
             | CurrentLogStats
             deriving stock (Show, Eq, Ord)

data Resource (layout :: Layout) where
  MainRes :: MainRes -> Resource 'Main

instance Show (Resource layout) where
  show (MainRes mainres) = show mainres

instance Eq (Resource layout) where
  (MainRes mainres1) == (MainRes mainres2) = mainres1 == mainres2

instance Ord (Resource layout) where
  (MainRes mainres1) <= (MainRes mainres2) = mainres1 <= mainres2

data MainState =
  MainState { _msCurrentLogL          :: Log
            , _msCurrentTimeL         :: Time
            , _msClockAnimationStateL :: Integer
            , _msCurrentWindowL       :: Resource 'Main
            } deriving stock (Show)

makeLenses ''MainState


data UI (layout :: Layout) where
  MainUI :: MainState -> UI 'Main

data Tick = Tick
            deriving stock (Show)
