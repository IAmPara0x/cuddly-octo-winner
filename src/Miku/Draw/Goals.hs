{-# LANGUAGE TemplateHaskell #-}

module Miku.Draw.Goals
  ( CompletedGoals(..)
  , NotCompletedGoals(..)
  ) where

import Brick.Types (Padding (Pad), Widget)

import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Control.Lens
  ( to
  , (^.)
  , makePrisms
  )

import Miku.Templates.Log
  ( Goal
  , goalDescL
  )

import Miku.Draw (Drawable(..), drawableL, borderTypeL)
import Miku.Mode (Name)

import Relude

newtype CompletedGoals    = CompletedGoals [Goal]
makePrisms ''CompletedGoals

newtype NotCompletedGoals = NotCompletedGoals [Goal]
makePrisms ''NotCompletedGoals

instance Drawable CompletedGoals where
  draw drawState = drawState ^. drawableL
                              . _CompletedGoals
                              . to (drawGoals "[✓] Completed")
                              . to (Core.withBorderStyle $ drawState ^. borderTypeL)

instance Drawable NotCompletedGoals where
  draw drawState = drawState ^. drawableL
                              . _NotCompletedGoals
                              . to (drawGoals "[✕] Not Completed")
                              . to (Core.withBorderStyle $ drawState ^. borderTypeL)

drawGoals :: Text -> [Goal] -> Widget Name
drawGoals heading goals =
    Border.border
  $ Core.center
  $ Core.vBox
      [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
      , Core.vBox (map drawGoal goals)
      , Core.fill ' '
      ]

drawGoal :: Goal -> Widget Name
drawGoal goal = Core.padBottom (Pad 1) $
  Core.hBox [ Core.padLeft (Pad 1)
            $ Core.txt $ goal ^. goalDescL
            ]
