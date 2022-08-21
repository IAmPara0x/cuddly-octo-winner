module Miku.UI.Draw.Goals
  ( CompletedGoals(..)
  , NotCompletedGoals(..)
  ) where

import Brick.Types (Padding (Pad), Widget)

import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Control.Lens
  ( ifolded
  , to
  , withIndex
  , _1
  , (^.)
  , (^..)
  , (+~)
  )

import Miku.Templates.Log
  ( Goal
  , goalDescL
  )

import Miku.UI.Draw (Drawable(..), Border(Rounded))
import Relude


newtype CompletedGoals = CompletedGoals [Goal]

instance Drawable CompletedGoals where
  draw border = drawGoals border "[✓] Completed" . coerce


newtype NotCompletedGoals = NotCompletedGoals [Goal]

instance Drawable NotCompletedGoals where
  draw border = drawGoals border "[✕] Not Completed" . coerce

drawGoals :: Border -> Text -> [Goal] -> Widget n
drawGoals border heading goals
  | border == Rounded = Core.withBorderStyle Border.unicodeRounded (Border.border goalsWidget)
  | otherwise         = Core.withBorderStyle (Border.borderStyleFromChar ' ') (Border.border goalsWidget)

  where

    goalsWidget :: Widget n
    goalsWidget =
      Core.center
        $ Core.vBox
           [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
           , Core.vBox
                  (goals ^.. ifolded . withIndex . to (uncurry drawGoal . (_1 +~ 1)))
           , Core.fill ' '
           ]

drawGoal :: Int -> Goal -> Widget n
drawGoal n goal = Core.padBottom (Pad 1) $
  Core.hBox [ Core.padLeftRight 1 $ Core.txt (show n <> ")")
            , Core.padLeft (Pad 1)
            $ Core.txt $ goal ^. goalDescL
            ]
