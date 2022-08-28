module Miku.Draw.CurrentTask
  ( CurrentTask(..)
  , CurrentTaskName(..)
  , CurrentTaskTags(..)
  , CurrentTaskDesc(..)
  )
  where

import Brick.Types (Padding (Pad), Widget)
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Brick.Widgets.Core ((<=>))

import Control.Lens ((^.))

import Data.Text qualified as Text

import Relude

import Miku.Templates.Log
  ( Task(..)
  , TaskName(TaskName)
  , TaskTag
  , TaskDesc
  , showTags
  , descL
  , nameL
  )
import Miku.Types.Time (Time, showTime)
import Miku.Draw (Drawable(..))
import Miku.Mode (Name)

import Prelude ((!!))


data CurrentTask = CurrentTask Int Task
                 | NoCurrentTask Text

instance Drawable CurrentTask where
  draw (NoCurrentTask text)   = Border.border $ noOngoinTaskWidget text
  draw (CurrentTask idx task) = Border.border $ ongoinTaskWidget idx task



newtype CurrentTaskName = CurrentTaskName TaskName

instance Drawable CurrentTaskName where
  draw (CurrentTaskName taskName) =drawTaskName <=> Border.hBorder 
    where
      drawTaskName :: Widget n
      drawTaskName =
          Core.vLimitPercent 15
        $ Core.padTopBottom 1
        $ Core.vBox
          [ Core.hCenter (Core.txt (taskName ^. nameL))
          ]

noOngoinTaskWidget :: Text -> Widget Name
noOngoinTaskWidget = Core.hLimitPercent 50
                   . Core.center
                   . Core.txt

ongoinTaskWidget :: Int -> Task -> Widget Name
ongoinTaskWidget idx Task{..} =
    Core.hLimitPercent 50
  $ Core.vBox
    [ draw (coerce @_ @CurrentTaskName _taskNameL)
    , Core.padTop (Pad 1)
      $ Core.vLimit 2
      $ Core.hBox [ draw (coerce @_ @StartTime _taskStartL)
                  , Core.fill ' '
                  , draw (EndTime idx _taskEndL)
                  ]
    , Core.padLeft (Pad 4) $ Core.padTopBottom 1
       $ draw (coerce @_ @CurrentTaskDesc _taskDescL)
    , draw (coerce @_ @CurrentTaskTags _taskTagsL)

    ]

newtype CurrentTaskDesc = CurrentTaskDesc (Maybe TaskDesc)

instance Drawable CurrentTaskDesc where
  draw (CurrentTaskDesc (Just desc)) = Core.vCenter $ Core.txt (desc ^. descL)
  draw (CurrentTaskDesc Nothing)     = Core.vCenter $ Core.txt ""

newtype StartTime = StartTime Time

instance Drawable StartTime where
  draw = Core.padLeft (Pad 1)
       . Core.txt
       . ("From: " <>)
       . showTime
       . coerce

data EndTime = EndTime Int (Maybe Time)

instance Drawable EndTime where
  draw (EndTime idx _) = Core.padRight (Pad 1)
                       $ Core.txt (Text.snoc "Ongoing: " $ clockAnimationStates !! idx) -- TODO: remove (!!).

newtype CurrentTaskTags = CurrentTaskTags [TaskTag]

instance Drawable CurrentTaskTags where
  draw (CurrentTaskTags tags) = Border.hBorder <=> drawTaskTags

    where

      drawTaskTags :: Widget n
      drawTaskTags = Core.padTopBottom 1 $ Core.txt $ showTags tags

clockAnimationStates :: [Char]
clockAnimationStates = '◴' : ['◷','◶','◵']
