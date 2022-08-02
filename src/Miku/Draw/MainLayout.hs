{-# LANGUAGE GADTs #-}
module Miku.Draw.MainLayout (drawUIMain, handleEventMain) where


import Brick.Widgets.Border        qualified as B
import Brick.Widgets.Border.Style  qualified as B
import Brick.Widgets.Center        qualified as C


import Graphics.Vty                qualified as V

import Prelude                     qualified as P

import Data.Text                   qualified as T

import Brick.Main
  ( ViewportScroll
  , continue
  , viewportScroll
  , vScrollBy
  )

import Brick.Types
  ( BrickEvent(AppEvent, VtyEvent)
  , EventM
  , Next
  , ViewportType(Vertical)
  , Widget
  )

import Brick.Util (fg)

import Control.Lens
  ( (^.)
  , (.~)
  , (%~)
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
  , viewport
  , withBorderStyle
  )


import Miku.Types.Log
import Miku.Types.Parser (showAtom, BluePrint)

import Miku.UI
  ( UI(MainUI)
  , Layout(..)
  , msClockAnimationStateL
  , msCurrentLogL
  , msCurrentTimeL
  , msCurrentWindowL
  , MainRes(..)
  , Resource(MainRes)
  , Tick(Tick)
  )

import Miku.UI.Utils
  ( emoji
  , fillEmpty
  , padLeft
  , padRight
  , padTop
  , padTopBottom
  , titleBar
  )

clockAnimationStates :: [Char]
clockAnimationStates = ['◴','◷','◶','◵','◴']

clockAnimation :: Integer -> Widget (Resource 'Main)
clockAnimation n = emoji (fg V.brightGreen) (clockAnimationStates P.!! fromInteger n)

drawHeading :: Heading -> Widget (Resource 'Main)
drawHeading = vLimitPercent 5 . titleBar . showAtom @(BluePrint Heading)

drawTaskName :: TaskName -> Widget (Resource 'Main)
drawTaskName taskName =
    padTopBottom 1
  $ C.hCenter
  $ txt $ "### " <> taskName ^. nameL

drawCurrentTaskTime :: Time -> Time -> Integer -> Widget (Resource 'Main)
drawCurrentTaskTime startTime currTime animState =
    vLimitPercent 10
  $ hBox
  [ padLeft 2 $ txt $ "started on: " <> showAtom @(BluePrint Time) startTime
  , fill ' '
  , padRight 2 $ txt ("ongoing: " <> showAtom @(BluePrint Time) (abs (currTime - startTime)) <> " ")
    <+> clockAnimation animState
  ]

drawTaskDesc :: Maybe TaskDesc -> Widget (Resource 'Main)
drawTaskDesc desc =
    padTopBottom 1
  $ padLeft 4
  $ C.vCenter
  $ txtWrap
  $ maybe "This Task has no description." (^. descL) desc


drawTaskTags :: [TaskTag] -> Widget (Resource 'Main)
drawTaskTags = padTopBottom 1 . padLeft 2 . txtWrap . T.init . showAtom @(BluePrint [TaskTag])

drawCurrentTask :: Maybe Task -> Time -> Integer -> Widget (Resource 'Main)
drawCurrentTask mtask currTime clockAnimState =
    hLimitPercent 55 $ vLimitPercent 50
  $ withBorderStyle B.unicodeRounded $ B.border
  $ vBox [ titleBar "Current Task"
         , maybe drawNoOngoinTask drawTask mtask
         ]

  where
    drawTask :: Task -> Widget (Resource 'Main)
    drawTask task =
      vBox [ drawTaskName (task ^. taskNameL)
           , drawCurrentTaskTime (task ^. taskStartL) currTime clockAnimState
           , withBorderStyle B.ascii B.hBorder
           , drawTaskDesc (task ^. taskDescL)
           , withBorderStyle B.ascii B.hBorder
           , drawTaskTags (task ^. taskTagsL)
           ]
    drawNoOngoinTask :: Widget (Resource 'Main)
    drawNoOngoinTask = C.center $ txt "There's currently not any ongoing task."

drawGoalStatus :: GoalStatus -> Widget (Resource 'Main)
drawGoalStatus Done    = padTopBottom 1 $ txt "[" <+> emoji (fg V.green) '✓' <+> txt "]"
drawGoalStatus NotDone = padTopBottom 1 $ txt "[" <+> emoji (fg V.red) '✕' <+> txt "]"


drawGoal :: Goal -> Widget (Resource 'Main)
drawGoal goal =
  hBox [ drawGoalStatus (goal ^. goalStatusL)
       , padTopBottom 1 $ padLeft 1
         $ txtWrap $ goal ^. goalDescL
       ]

drawCompletedGoals :: [Goal] -> Widget (Resource 'Main)
drawCompletedGoals goals =
    vLimitPercent 100
  $ viewport (MainRes CompletedGoals) Vertical
  $ vBox
     $ [ padTop 1 $ C.hCenter $ txt "Completed"
       , withBorderStyle B.ascii B.hBorder
       ] <> map drawGoal goals

drawNotCompletedGoals :: [Goal] -> Widget (Resource 'Main)
drawNotCompletedGoals goals =
      vLimitPercent 50
    $ viewport (MainRes NotCompletedGoals) Vertical
    $ vBox
    $ [ padTop 1 $ C.hCenter $ txt "Not Completed"
      , withBorderStyle B.ascii B.hBorder
      ] <> map drawGoal goals

drawTodaysGoals :: [Goal] -> Widget (Resource 'Main)
drawTodaysGoals goals =
      vLimitPercent 50
    $ withBorderStyle B.unicodeRounded
    $ B.border
    $ vBox [ titleBar "Today's Goals"
           , drawNotCompletedGoals (goalsNotDone goals)
           , fillEmpty
           , withBorderStyle B.unicode B.hBorder
           , drawCompletedGoals (goalsDone goals)
           , fillEmpty
           ]

drawUIMain :: UI 'Main -> [Widget (Resource 'Main)]
drawUIMain (MainUI ms) =
   [ vBox [  drawHeading     (ms ^. (msCurrentLogL . logHeadingL))

          ,  drawCurrentTask (ongoingTask (ms ^. msCurrentLogL))
                             (ms ^. msCurrentTimeL)
                             (ms ^. msClockAnimationStateL)
         <+> drawTodaysGoals (ms ^. (msCurrentLogL . logGoalsL))
          ]
   ]


updateUI :: UI 'Main -> IO (UI 'Main)
updateUI (MainUI ms) = do
   time <- liftIO getCurrentTime
   return $ MainUI $ ms & msCurrentTimeL .~ time
                        & msClockAnimationStateL  %~ (\n -> mod (n + 1) 4)

scrollCurrentWindow :: UI 'Main -> ViewportScroll (Resource 'Main)
scrollCurrentWindow (MainUI ms) = viewportScroll (ms ^. msCurrentWindowL)

changeCurrentWindow :: UI 'Main -> UI 'Main
changeCurrentWindow (MainUI ms) = MainUI (ms & msCurrentWindowL .~ MainRes CompletedGoals)

handleEventMain :: UI 'Main -> BrickEvent (Resource 'Main) Tick -> EventM (Resource 'Main) (Next (UI 'Main))
handleEventMain ui (AppEvent Tick)                           = liftIO (updateUI ui)
                                                           >>= continue
handleEventMain ui (VtyEvent (V.EvKey (V.KChar 'j') []))     = vScrollBy (scrollCurrentWindow ui) 1
                                                            >> continue ui
handleEventMain ui (VtyEvent (V.EvKey (V.KChar 'k') []))     = vScrollBy (scrollCurrentWindow ui) (-1)
                                                            >> continue ui
handleEventMain ui (VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl]))    = continue (changeCurrentWindow ui)
handleEventMain ui            _                              = continue ui
