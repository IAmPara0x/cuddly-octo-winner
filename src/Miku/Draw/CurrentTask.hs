module Miku.Draw.CurrentTask
  ( CurrentTask(..)
  , CurrentTaskName(..)
  , CurrentTaskTags(..)
  , CurrentTaskDesc(..)
  )
  where

import           Brick.Types          (Padding (Pad), Widget)
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Core
import qualified Brick.Widgets.Core   as Core

import           Brick.Widgets.Core   ((<=>))

import           Control.Lens         (_1, makePrisms, to, (.~), (^.))

import qualified Data.Text            as Text

import           Relude

import           Miku.Draw            (Drawable (..), borderTypeL, drawableL)
import           Miku.Mode            (Name)
import           Miku.Templates.Log   (Task (..), TaskDesc, TaskName, TaskTag,
                                       descL, nameL, showTags)
import           Miku.Types.Time      (Time, showTime)

import           Relude.Unsafe        ((!!))

newtype CurrentTaskName = CurrentTaskName TaskName
makePrisms ''CurrentTaskName

instance Drawable CurrentTaskName where
  draw drawState = drawState ^. drawableL
                              . _CurrentTaskName
                              . to drawTaskName
                              . to (<=> Border.hBorder)
                              . to (Core.withBorderStyle $ drawState ^. borderTypeL)

    where drawTaskName :: TaskName -> Widget n
          drawTaskName taskName =
              Core.vLimitPercent 15
            $ Core.padTopBottom 1
            $ Core.vBox
              [ Core.hCenter (Core.txt (taskName ^. nameL))
              ]

newtype CurrentTaskDesc = CurrentTaskDesc (Maybe TaskDesc)
makePrisms ''CurrentTaskDesc

instance Drawable CurrentTaskDesc where
  draw drawState = drawState ^. drawableL
                              . _CurrentTaskDesc
                              . to (maybe "" (^. descL))
                              . to (Core.vCenter . Core.txt)

newtype StartTime = StartTime Time
makePrisms ''StartTime

instance Drawable StartTime where
  draw drawState = drawState ^. drawableL
                              . _StartTime
                              . to showTime
                              . to ("From: " <>)
                              . to Core.txt
                              . to (Core.padLeft $ Pad 1)


data EndTime = EndTime Int (Maybe Time)
makePrisms ''EndTime

instance Drawable EndTime where
  draw drawState = drawState ^. drawableL
                              . _EndTime
                              . _1
                              . to (clockAnimationStates !!)
                              . to (Text.snoc "Ongoing: ")
                              . to Core.txt
                              . to (Core.padRight $ Pad 1)

clockAnimationStates :: [Char]
clockAnimationStates = '◴' : ['◷','◶','◵']

newtype CurrentTaskTags = CurrentTaskTags [TaskTag]
makePrisms ''CurrentTaskTags

instance Drawable CurrentTaskTags where
  draw drawState = drawState ^. drawableL
                              . _CurrentTaskTags
                              . to drawTaskTags
                              . to (Border.hBorder <=>)
    where
      drawTaskTags tags = Core.padTopBottom 1 $ Core.txt $ showTags tags

data CurrentTask = CurrentTask Int Task
                 | NoCurrentTask Text

instance Drawable CurrentTask where
  draw drawState =
    case drawState ^. drawableL of
      NoCurrentTask msg        -> Core.withBorderStyle (drawState ^. borderTypeL)
                                $ Border.border $ noOngoinTaskWidget msg
      CurrentTask idx Task{..} -> Core.withBorderStyle (drawState ^. borderTypeL)
                                $ Border.border $ Core.hLimitPercent 50
                                $ Core.vBox
                                  [ draw $ drawState & drawableL .~ CurrentTaskName _taskNameL
                                  , Core.padTop (Pad 1)
                                    $ Core.vLimit 2
                                    $ Core.hBox [ draw $ drawState & drawableL .~ StartTime _taskStartL
                                                , Core.fill ' '
                                                , draw $ drawState & drawableL .~ EndTime idx _taskEndL
                                                ]
                                  , Core.padLeft (Pad 4) $ Core.padTopBottom 1
                                    $ draw $ drawState & drawableL .~ CurrentTaskDesc _taskDescL
                                  , draw $ drawState & drawableL .~ CurrentTaskTags _taskTagsL
                                  ]

noOngoinTaskWidget :: Text -> Widget Name
noOngoinTaskWidget = Core.hLimitPercent 50
                   . Core.center
                   . Core.txt
