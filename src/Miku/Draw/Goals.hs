{-# LANGUAGE NamedFieldPuns #-}

module Miku.Draw.Goals
  ( CompletedGoals(..)
  , _CompletedGoals
  , NotCompletedGoals(..)
  , _NotCompletedGoals
  ) where

import Brick.Types (Padding (Pad), Widget)

import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Control.Lens
  ( to
  , makePrisms
  , ifolded
  , withIndex
  , (^.)
  , (^..)
  )

import Miku.Templates.Log
  ( Goal
  , goalDescL
  )

import Miku.Draw (Drawable(..), Draw(..))
import Miku.Mode (Name)

import Relude

data CompletedGoals    = CompletedGoals Int [Goal]
makePrisms ''CompletedGoals

data NotCompletedGoals = NotCompletedGoals Int [Goal]
makePrisms ''NotCompletedGoals

instance Drawable CompletedGoals where
  draw Draw{ _drawableL=CompletedGoals currIdx goals , .. }
    = Core.withBorderStyle _borderTypeL $ drawGoals "[✓] Completed"

    where drawGoals heading =
            Border.border
            $ Core.center
            $ Core.vBox
                [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
                , Core.vBox
                    (goals ^.. ifolded . withIndex . to addAttr)
                , Core.fill ' '
                ]

          addAttr (idx, goal)
              | not _focusedL  = Core.withAttr "goal"    $ drawGoal goal
              | idx == currIdx = Core.withAttr "current" $ drawGoal goal
              | otherwise      = drawGoal goal

instance Drawable NotCompletedGoals where
  draw Draw{ _drawableL=NotCompletedGoals currIdx goals , .. }
    = Core.withBorderStyle _borderTypeL $ drawGoals "[✕] Not Completed"
    where drawGoals heading =
            Border.border
            $ Core.center
            $ Core.vBox
                [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
                , Core.vBox
                    (goals ^.. ifolded . withIndex . to addAttr)
                , Core.fill ' '
                ]

          addAttr (idx, goal)
              | not _focusedL  = Core.withAttr "goal"    $ drawGoal goal
              | idx == currIdx = Core.withAttr "current" $ drawGoal goal
              | otherwise      = drawGoal goal

drawGoal :: Goal -> Widget Name
drawGoal goal = Core.padBottom (Pad 1) $
  Core.hBox [ Core.padLeft (Pad 1)
            $ Core.txt $ goal ^. goalDescL
            ]
