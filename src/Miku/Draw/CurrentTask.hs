module Miku.Draw.CurrentTask
  ( CurrentTask (..)
  , CurrentTaskDesc (..)
  , CurrentTaskItem (..)
  , CurrentTaskName (..)
  , CurrentTaskTags (..)
  , changeCurrentTaskFocus
  ) where

import Brick.Types          (Padding (Pad), Widget)
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Brick.Widgets.Core   ((<=>))

import Control.Lens         (_1, ix, makePrisms, to, (%~), (.~), (^.))

import Data.Text            qualified as Text

import Relude

import Miku.Draw            (Draw (..), Drawable (..), borderTypeL, drawableL)
import Miku.Draw.StatusLine (StatusLineInfo (..))
import Miku.Mode            (Name)
import Miku.Templates.Log   (Task (..), TaskDesc, TaskName, TaskTag, descL, nameL, showTags)
import Miku.Types.Time      (Time, showTime)

import Relude.Unsafe        ((!!))

newtype CurrentTaskName
  = CurrentTaskName TaskName
makePrisms ''CurrentTaskName

instance Drawable CurrentTaskName where
  draw drawState =
    drawState ^. drawableL . _CurrentTaskName . to drawTaskName . to (<=> Border.hBorder) . to
      (Core.withBorderStyle $ drawState ^. borderTypeL)

   where
    drawTaskName :: TaskName -> Widget n
    drawTaskName taskName = Core.vLimitPercent 15 $ Core.padTopBottom 1 $ Core.vBox
      [Core.hCenter $ Core.txt $ taskName ^. nameL]

newtype CurrentTaskDesc
  = CurrentTaskDesc (Maybe TaskDesc)
makePrisms ''CurrentTaskDesc

instance Drawable CurrentTaskDesc where
  draw drawState =
    drawState ^. drawableL . _CurrentTaskDesc . to (maybe "No Description." (^. descL)) . to
      (Core.vCenter . Core.txt)

newtype StartTime
  = StartTime Time
makePrisms ''StartTime

instance Drawable StartTime where
  draw drawState =
    drawState ^. drawableL . _StartTime . to showTime . to ("From: " <>) . to Core.txt . to
      (Core.padLeft $ Pad 1)


data EndTime
  = EndTime Int (Maybe Time)
makePrisms ''EndTime

instance Drawable EndTime where
  draw drawState =
    drawState
      ^. drawableL
      .  _EndTime
      .  _1
      .  to (clockAnimationStates !!)
      .  to (Text.snoc "Ongoing: ")
      .  to Core.txt
      .  to (Core.padRight $ Pad 1)

clockAnimationStates :: [Char]
clockAnimationStates = '◴' : ['◷', '◶', '◵']

newtype CurrentTaskTags
  = CurrentTaskTags [TaskTag]
makePrisms ''CurrentTaskTags

instance Drawable CurrentTaskTags where
  draw drawState = drawState ^. drawableL . _CurrentTaskTags . to drawTaskTags . to
    (Border.hBorder <=>)
    where drawTaskTags tags = Core.padTopBottom 1 $ Core.txt $ showTags tags

data CurrentTaskItem
  = TaskName
  | TaskStartTime
  | TaskDesc
  | TaskTags
  deriving stock (Bounded, Enum, Show)

instance StatusLineInfo CurrentTaskItem where
  statusLineInfo TaskName      = ["Name"]
  statusLineInfo TaskDesc      = ["Desc"]
  statusLineInfo TaskStartTime = ["Start"]
  statusLineInfo TaskTags      = ["Tags"]

data CurrentTask
  = CurrentTask CurrentTaskItem Task
  | NoCurrentTask Text

changeCurrentTaskFocus :: Int -> CurrentTask -> CurrentTask
changeCurrentTaskFocus n (CurrentTask item task) =
  CurrentTask (toEnum $ mod (fromEnum item + n) (fromEnum @CurrentTaskItem maxBound + 1)) task
changeCurrentTaskFocus _ t = t

instance StatusLineInfo CurrentTask where
  statusLineInfo (CurrentTask item _) = ["CurrentTask"] <> statusLineInfo item
  statusLineInfo NoCurrentTask{}      = ["NoCurrentTask"]

instance Drawable CurrentTask where
  draw drawState@Draw {..} = case _drawableL of
    NoCurrentTask msg -> Core.withBorderStyle _borderTypeL $ Border.border $ noOngoinTaskWidget msg
    CurrentTask item Task {..} ->
      Core.withBorderStyle _borderTypeL
        $ Border.border
        $ Core.hLimitPercent 50
        $ Core.vBox
        $ addAttr
            _focusedL
            item
            [ draw $ drawState & drawableL .~ CurrentTaskName _taskNameL
            , Core.padTop (Pad 1) $ Core.vLimit 2 $ Core.hBox
              [ draw $ drawState & drawableL .~ StartTime _taskStartL
              , Core.fill ' '
              , draw $ drawState & drawableL .~ EndTime 0 _taskEndL
              ]
            , Core.padLeft (Pad 4)
            $  Core.padTopBottom 1
            $  draw
            $  drawState
            &  drawableL
            .~ CurrentTaskDesc _taskDescL
            , draw $ drawState & drawableL .~ CurrentTaskTags _taskTagsL
            ]
   where
    addAttr :: Bool -> CurrentTaskItem -> [Widget Name] -> [Widget Name]
    addAttr False _    widgets = widgets
    addAttr True  item widgets = widgets & ix (fromEnum item) %~ Core.withAttr "current"

    noOngoinTaskWidget :: Text -> Widget Name
    noOngoinTaskWidget = Core.hLimitPercent 50 . Core.center . Core.txt
