module Miku.UI.Draw.Goals (drawCompletedGoals, drawNotCompletedGoals) where

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
  , goalsDone
  , goalsNotDone
  )

import Relude


drawCompletedGoals :: Bool -> [Goal] -> Widget n
drawCompletedGoals drawBorder = drawGoals drawBorder "[✓] Completed" . goalsDone

drawNotCompletedGoals :: Bool -> [Goal] -> Widget n
drawNotCompletedGoals drawBorder = drawGoals drawBorder "[✕] Not Completed" . goalsNotDone
          
drawGoals :: Bool -> Text -> [Goal] -> Widget n
drawGoals drawBorder heading goals
  | drawBorder  = Core.withBorderStyle Border.unicodeRounded (Border.border goalsWidget)
  | otherwise   = Core.withBorderStyle (Border.borderStyleFromChar ' ') (Border.border goalsWidget)

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
drawGoal n goal = Core.padTopBottom 1 $
  Core.hBox [ Core.padLeftRight 1 $ Core.txt (show n <> ")")
            , Core.padLeft (Pad 1)
            $ Core.txtWrap $ goal ^. goalDescL
            ]
