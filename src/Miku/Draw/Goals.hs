module Miku.Draw.Goals
  ( CompletedGoals(..)
  , NotCompletedGoals(..)
  ) where

import Brick.Types (Padding (Pad), Widget)

import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Control.Lens
  ( folded
  , to
  , (^.)
  , (^..)
  )

import Miku.Templates.Log
  ( Goal
  , goalDescL
  )

import Miku.Draw (Drawable(..))
import Relude


newtype CompletedGoals    = CompletedGoals [Goal]

instance Drawable CompletedGoals where
  draw = drawGoals "[✓] Completed" . coerce


newtype NotCompletedGoals = NotCompletedGoals [Goal]

instance Drawable NotCompletedGoals where
  draw = drawGoals "[✕] Not Completed" . coerce

drawGoals :: Text -> [Goal] -> Widget n
drawGoals heading goals = Border.border goalsWidget

  where

    goalsWidget :: Widget n
    goalsWidget =
      Core.center
        $ Core.vBox
           [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
           , Core.vBox
                  (goals ^.. folded . to drawGoal)
           , Core.fill ' '
           ]

drawGoal :: Goal -> Widget n
drawGoal goal = Core.padBottom (Pad 1) $
  Core.hBox [ Core.padLeft (Pad 1)
            $ Core.txt $ goal ^. goalDescL
            ]
