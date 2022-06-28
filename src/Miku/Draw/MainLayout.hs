{-# LANGUAGE GADTs #-}
module Miku.Draw.MainLayout (drawUIMain, handleEventMain) where


import Brick.Widgets.Border        qualified as B
import Brick.Widgets.Border.Style  qualified as B
import Brick.Widgets.Center        qualified as C


import Graphics.Vty                qualified as V

import Prelude                     qualified as P

import Data.Text                   qualified as T

import Brick.Main (continue)
import Brick.Types
  ( BrickEvent(AppEvent)
  , EventM
  , Next
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
  , ResourceName
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

clockAnimation :: Integer -> Widget n
clockAnimation n = emoji (fg V.brightGreen) (clockAnimationStates P.!! fromInteger n)


drawHeading :: Heading -> Widget n
drawHeading = vLimitPercent 5 . titleBar . showAtom @(BluePrint Heading)

drawTaskName :: TaskName -> Widget n
drawTaskName taskName =
    padTopBottom 1
  $ C.hCenter
  $ txt $ "### " <> taskName ^. nameL

drawCurrentTaskTime :: Time -> Time -> Integer -> Widget n
drawCurrentTaskTime startTime currTime animState =
    vLimitPercent 10
  $ hBox
  [ padLeft 2 $ txt $ "Started On: " <> showAtom @(BluePrint Time) startTime
  , fill ' '
  , padRight 2 $ txt ("Ongoing: " <> showAtom @(BluePrint Time) (abs (currTime - startTime)) <> " ")
    <+> clockAnimation animState
  ]

drawTaskDesc :: Maybe TaskDesc -> Widget n
drawTaskDesc desc =
    padTopBottom 1 $ padLeft 4
  $ C.vCenter
  $ txtWrap
  $ maybe "This Task has no description." (^. descL) desc


drawTaskTags :: [TaskTag] -> Widget n
drawTaskTags = padTopBottom 1 . padLeft 2 . txtWrap . T.filter (/= '*') . showAtom @(BluePrint [TaskTag])

drawCurrentTask :: Maybe Task -> Time -> Integer -> Widget n
drawCurrentTask mtask currTime clockAnimState =
    hLimitPercent 60 $ vLimitPercent 50
  $ withBorderStyle B.unicodeRounded $ B.border
  $ vBox [ titleBar "Current Task"
         , maybe drawNoOngoinTask drawTask mtask
         ]

  where
    drawTask :: Task -> Widget n
    drawTask task =
      vBox [ drawTaskName (task ^. taskNameL)
           , drawCurrentTaskTime (task ^. taskStartL) currTime clockAnimState
           , withBorderStyle B.ascii B.hBorder
           , drawTaskDesc (task ^. taskDescL)
           , withBorderStyle B.ascii B.hBorder
           , drawTaskTags (task ^. taskTagsL)
           ]
    drawNoOngoinTask :: Widget n
    drawNoOngoinTask = C.center $ txt "There's current not any ongoing task."

drawGoalStatus :: GoalStatus -> Widget n
drawGoalStatus Done    = padTopBottom 1 $ txt "[" <+> emoji (fg V.green) '✓' <+> txt "]"
drawGoalStatus NotDone = padTopBottom 1 $ txt "[" <+> emoji (fg V.red) '✕' <+> txt "]"


drawGoal :: Goal -> Widget n
drawGoal goal =
  hBox [ drawGoalStatus (goal ^. goalStatusL)
       , padTopBottom 1 $ padLeft 1
         $ txtWrap $ goal ^. goalDescL
       ]

drawCompletedGoals :: [Goal] -> Widget n
drawCompletedGoals goals =
  vBox
     $ [ padTop 1 $ C.hCenter $ txt "Completed"
       , withBorderStyle B.ascii B.hBorder
       ] <> map drawGoal goals

drawNotCompletedGoals :: [Goal] -> Widget n
drawNotCompletedGoals goals =
  vBox
    $ [ padTop 1 $ C.hCenter $ txt "Not Completed"
      , withBorderStyle B.ascii B.hBorder
      ] <> map drawGoal goals

drawTodaysGoals :: [Goal] -> Widget n
drawTodaysGoals goals =
    vLimitPercent 50 $ withBorderStyle B.unicodeRounded $ B.border
    $ vBox [ titleBar "Todays' Goals"
           , drawNotCompletedGoals (goalsNotDone goals)
           , fillEmpty
           , withBorderStyle B.unicode B.hBorder
           , drawCompletedGoals (goalsDone goals)
           , fillEmpty
           ]

drawUIMain :: UI 'Main -> [Widget ResourceName]
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

handleEventMain :: UI 'Main -> BrickEvent ResourceName Tick -> EventM ResourceName (Next (UI 'Main))
handleEventMain ui (AppEvent Tick)   = liftIO (updateUI ui) >>= continue
handleEventMain ui            _      = continue ui
