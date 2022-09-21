module Miku.Draw.CurrentTask
  ( CurrentTask (..)
  , CurrentTaskItem (..)
  , Stats (Stats)
  ) where

import Brick.Types                (Padding (Pad), Widget)
import Brick.Widgets.Border       qualified as Border
import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Center       qualified as Core
import Brick.Widgets.Core         qualified as Core

import Brick.Widgets.Core         ((<=>))

import Control.Lens               (ix, (%~), (^.))

import Data.Text                  qualified as Text


import Miku.Draw                  (InitWidget, W (..))
import Miku.Draw                  qualified as Draw
import Miku.Draw.StatusLine       (StatusInfo)
import Miku.Resource              (Res)
import Miku.Templates.Log         (Task (..), TaskDesc, TaskName, TaskTag)
import Miku.Templates.Log         qualified as Log
import Miku.Types.Time            (Time)
import Miku.Types.Time            qualified as Time

import Relude

data CurrentTaskItem
  = TaskName
  | TaskStartTime
  | TaskDesc
  | TaskTags
  deriving stock (Bounded, Enum, Eq, Show)

data CurrentTask
  = CurrentTask CurrentTaskItem Task
  | NoCurrentTask Text
  deriving stock (Eq)

instance InitWidget CurrentTask where
  initWidget a = W { _focusedL        = False
                   , _borderTypeL     = Border.borderStyleFromChar ' '
                   , _widgetStateL    = a
                   , _drawL           = drawCurrentTask
                   , _statusLineInfoL = currentTaskInfo . _widgetStateL
                   , _changeFocusL    = \n -> Draw.widgetStateL %~ changeCurrentTaskFocus n
                   }

changeCurrentTaskFocus :: Int -> CurrentTask -> CurrentTask
changeCurrentTaskFocus n (CurrentTask item task) =
  CurrentTask (toEnum $ mod (fromEnum item + n) (fromEnum @CurrentTaskItem maxBound + 1)) task
changeCurrentTaskFocus _ t = t

currentTaskInfo :: CurrentTask -> StatusInfo
currentTaskInfo (NoCurrentTask _   ) = "CurrentTask"
currentTaskInfo (CurrentTask item _) = "CurrentTask" <> itemInfo item
 where
  itemInfo TaskName      = "Name"
  itemInfo TaskDesc      = "Desc"
  itemInfo TaskStartTime = "Start"
  itemInfo TaskTags      = "Tags"

drawCurrentTask :: W CurrentTask -> Widget Res
drawCurrentTask W {..} = Core.withBorderStyle _borderTypeL $ Border.border $ case _widgetStateL of
  NoCurrentTask msg          -> noOngoinTaskWidget msg
  CurrentTask item Task {..} -> Core.hLimitPercent 50 $ Core.vBox $ addAttr
    _focusedL
    item
    [ drawTaskName _taskNameL
    , Core.padTop (Pad 1) $ Core.vLimit 2 $ Core.hBox
      [drawStartTime _taskStartL, Core.fill ' ', drawEndTime]
    , drawTaskDesc _taskDescL
    , drawTaskTags _taskTagsL
    ]
 where
  addAttr :: Bool -> CurrentTaskItem -> [Widget Res] -> [Widget Res]
  addAttr False _    widgets = widgets
  addAttr True  item widgets = widgets & ix (fromEnum item) %~ Core.withAttr "current"

  noOngoinTaskWidget :: Text -> Widget Res
  noOngoinTaskWidget = Core.hLimitPercent 50 . Core.center . Core.txt

drawTaskName :: TaskName -> Widget Res
drawTaskName taskName =
  Core.vLimitPercent
      15
      (Core.padTopBottom 1 $ Core.vBox [Core.hCenter $ Core.txt $ taskName ^. Log.nameL])
    <=> Border.hBorder

drawStartTime :: Time -> Widget Res
drawStartTime t = Core.padLeft (Pad 1) $ Core.txt ("From: " <> Time.showTime t)

drawEndTime :: Widget Res
drawEndTime = Core.padRight (Pad 1) $ Core.txt (Text.snoc "Ongoing " $ head clockAnimationStates)

clockAnimationStates :: NonEmpty Char
clockAnimationStates = '◴' :| ['◷', '◶', '◵']

drawTaskDesc :: Maybe TaskDesc -> Widget Res
drawTaskDesc desc = Core.padLeft (Pad 4) $ Core.padTopBottom 1 $ Core.vCenter $ Core.txt
  (maybe "No Description" (^. Log.descL) desc)

drawTaskTags :: [TaskTag] -> Widget Res
drawTaskTags tags = Border.hBorder <=> Core.padTopBottom 1 (Core.txt $ Log.showTags tags)

newtype Stats
  = Stats (Widget Res)

instance InitWidget Stats where
  initWidget a = W { _focusedL        = False
                   , _borderTypeL     = Border.borderStyleFromChar ' '
                   , _widgetStateL    = a
                   , _drawL           = drawStats
                   , _statusLineInfoL = const "Stats"
                   , _changeFocusL    = const id
                   }

drawStats :: W Stats -> Widget Res
drawStats W {..} = Core.withBorderStyle _borderTypeL $ coerce _widgetStateL
