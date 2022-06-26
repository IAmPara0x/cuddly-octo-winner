{-# LANGUAGE GADTs #-}
module Miku.Draw.Main (drawMainUI) where


import Brick.Widgets.Border        qualified as B
import Brick.Widgets.Border.Style  qualified as B
import Brick.Widgets.Center        qualified as C

import Brick.Types
  ( Widget
  )

import Control.Lens
  ( (^.)
  )

import Relude

import Brick.Widgets.Core
  ( (<+>)
  , fill
  , hBox
  , hLimitPercent
  , txt
  , txtWrap
  , vBox
  , vLimitPercent
  , withBorderStyle
  )


import Miku.Types.Log
import Miku.Types.Parser (showAtom, BluePrint)

import Miku.UI
  ( UI(MainUI)
  , Layout(..)
  , msCurrentLogL
  , ResourceName
  )

import Miku.UI.Utils
  ( emojiWidth2
  , fillEmpty
  , padLeft
  , padRight
  , padTop
  , padTopBottom
  , titleBar
  )

drawHeading :: Heading -> Widget n
drawHeading = vLimitPercent 5 . titleBar . showAtom @(BluePrint Heading)

drawTaskName :: TaskName -> Widget n
drawTaskName taskName = padTopBottom 1
                       $ C.hCenter
                       $ txt $ "### " <> taskName ^. nameL

drawCurrentTaskTime :: Time -> Widget n
drawCurrentTaskTime startTime = vLimitPercent 8
                              $ hBox
                                [ padLeft 2 $ txt $ "From: " <> showAtom @(BluePrint Time) startTime
                                , fill ' '
                                , padRight 2 $ txt "Ongoing: " <+> emojiWidth2 "🕗"
                                ]

drawTaskDesc :: Maybe TaskDesc -> Widget n
drawTaskDesc desc = padTopBottom 1 $ padLeft 4
                  $ C.vCenter
                  $ txtWrap
                  $ maybe "This Task Has no description." (^. descL) desc


drawCurrentTask :: Maybe Task -> Widget n
drawCurrentTask mtask = hLimitPercent 60 $ vLimitPercent 50
                      $ withBorderStyle B.unicodeRounded $ B.border
                      $ vBox [ titleBar "Current Task"
                             , maybe drawNoOngoinTask drawTask mtask
                             ]

  where
    drawTask :: Task -> Widget n
    drawTask task = vBox [ drawTaskName (task ^. taskNameL)
                         , drawCurrentTaskTime (task ^. taskStartL)
                         , withBorderStyle B.ascii B.hBorder
                         , drawTaskDesc (task ^. taskDescL)
                         , withBorderStyle B.ascii B.hBorder
                         , padTopBottom 1 $ padLeft 2 $ txt "Tags: Haskell, Miku"
                         ]
    drawNoOngoinTask :: Widget n
    drawNoOngoinTask = C.center $ txt "There's current not any ongoing task."

drawGoalStatus :: GoalStatus -> Widget n
drawGoalStatus Done    = padTopBottom 1 $ txt "[" <+> emojiWidth2 "✅" <+> txt "]"
drawGoalStatus NotDone = padTopBottom 1 $ txt "[" <+> emojiWidth2 "❌" <+> txt "]"


drawGoal :: Goal -> Widget n
drawGoal goal = hBox [ drawGoalStatus (goal ^. goalStatusL)
                     , padTopBottom 1 $ padLeft 1
                       $ txtWrap $ goal ^. goalDescL
                     ]

drawCompletedGoals :: [Goal] -> Widget n
drawCompletedGoals goals = vBox
                         $ [ padTop 1 $ C.hCenter $ txt "Completed"
                           , withBorderStyle B.ascii B.hBorder
                           ] <> map drawGoal goals

drawNotCompletedGoals :: [Goal] -> Widget n
drawNotCompletedGoals goals = vBox
                            $ [ padTop 1 $ C.hCenter $ txt "Not Completed"
                              , withBorderStyle B.ascii B.hBorder
                              ] <> map drawGoal goals

drawTodaysGoals :: [Goal] -> Widget n
drawTodaysGoals goals = vLimitPercent 50 $ withBorderStyle B.unicodeRounded $ B.border
                      $ vBox [ titleBar "Todays' Goals"
                             , drawNotCompletedGoals (goalsNotDone goals)
                             , fillEmpty
                             , withBorderStyle B.unicode B.hBorder
                             , drawCompletedGoals (goalsDone goals)
                             , fillEmpty
                             ]

drawMainUI :: UI 'Main -> [Widget ResourceName]
drawMainUI (MainUI ms) = [ vBox [  drawHeading     (ms ^. (msCurrentLogL . logHeadingL))
                                 ,  drawCurrentTask (ongoingTask (ms ^. msCurrentLogL))
                                <+> drawTodaysGoals (ms ^. (msCurrentLogL . logGoalsL))
                                ]
                         ]
