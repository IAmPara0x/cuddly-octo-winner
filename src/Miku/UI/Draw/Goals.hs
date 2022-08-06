module Miku.UI.Draw.Goals (drawGoals) where

import Brick.Types (Padding (Pad), Widget)

import Brick.Util                  qualified as Brick
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core ( (<+>)
                          )
import Brick.Widgets.Core          qualified as Core

import Control.Lens
  ( ifolded
  , to
  , withIndex
  , (^.)
  , (^..)
  )

import Graphics.Vty                qualified as Vty

import Miku.Templates.Log
  ( Goal
  , GoalStatus(Done, NotDone)
  , goalDescL
  , goalStatusL
  , goalsDone
  , goalsNotDone
  )

import Relude

drawGoals :: [Goal] -> Widget n
drawGoals goals = Core.hLimitPercent 65 $ Core.padTop (Pad 1) $
  Core.hBox [ Core.padLeft (Pad 1) completedGoals
            , Core.padLeft (Pad 1) notCompletedGoals 
            ]

  where
    completedGoals =
      -- Core.withBorderStyle Border.unicodeRounded $
      -- Border.border
         Core.center
        $ Core.vBox
           [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt "[✓] Completed"
           , Core.vBox
                   (goals ^.. to goalsDone . ifolded . withIndex . to (uncurry drawGoal))
           , Core.fill ' '
           ]
               

    notCompletedGoals =
      -- Core.withBorderStyle Border.unicodeRounded $
      -- Border.border
         Core.center
        $ Core.vBox
           [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt "[✕] Not Completed"
           , Core.vBox
                  (goals ^.. to goalsNotDone . ifolded . withIndex . to (uncurry drawGoal))
           , Core.fill ' '
           ]
          

drawGoal :: Int -> Goal -> Widget n
drawGoal n goal = Core.padTopBottom 1 $
  Core.hBox [ Core.padLeftRight 1 $ Core.txt (show n <> ")")
            , Core.padLeft (Pad 1)
            $ Core.txtWrap $ goal ^. goalDescL
            ]

drawGoalStatus :: Text -> Widget n
drawGoalStatus s    = Core.txt "[" <+> Core.txt s <+> Core.txt "]"
