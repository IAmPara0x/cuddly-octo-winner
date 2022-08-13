module Miku.UI.Draw.CurrentTask
  ( drawCurrentTask
  )
  where

import Brick.Types (Padding (Pad), Widget)
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Brick.Widgets.Core ((<=>))

import Control.Lens ((^.))

import Data.Text qualified as Text

import Relude

import Miku.Templates.Log
  ( Task
  , TaskName
  , TaskDesc
  , TaskTag
  
  , showTags
  , descL
  , nameL
  , taskNameL
  , taskDescL
  , taskStartL
  , taskTagsL
  )
import Miku.Types.Time (Time, showTime)


clockAnimationStates :: NonEmpty Char
clockAnimationStates = '◴' :| ['◶','◵','◴']

drawCurrentTask :: Bool -> Either Text Task -> Widget n
drawCurrentTask drawBorder (Left text)
  | drawBorder = Core.withBorderStyle Border.unicodeRounded $ Border.border $ noOngoinTaskWidget text
  | otherwise  = Core.withBorderStyle (Border.borderStyleFromChar ' ') $ Border.border $ noOngoinTaskWidget text

drawCurrentTask drawBorder (Right task)
  | drawBorder = Core.withBorderStyle Border.unicodeRounded
               $ Border.border
               $ ongoinTaskWidget drawBorder task
  | otherwise  = Core.withBorderStyle (Border.borderStyleFromChar ' ')
               $ Border.border
               $ ongoinTaskWidget drawBorder task

noOngoinTaskWidget :: Text -> Widget n
noOngoinTaskWidget = Core.hLimitPercent 50
                   . Core.center
                   . Core.txt

ongoinTaskWidget :: Bool -> Task -> Widget n
ongoinTaskWidget drawBorder task =
    Core.hLimitPercent 50
  $ Core.vBox
    [ taskNameWidget drawBorder (task ^. taskNameL)
    , Core.padTop (Pad 1)
      $ Core.vLimit 2 $ Core.hBox [startTimeWidget (task ^. taskStartL), Core.fill ' ', endTimeWidget]
    , Core.padLeft (Pad 4) $ Core.padTopBottom 1
       $ taskDescWidget (task ^. taskDescL)
    , taskTagsWidget drawBorder (task ^. taskTagsL)

    ]

taskNameWidget :: Bool -> TaskName -> Widget n
taskNameWidget drawBorder taskName
  | drawBorder = Core.withBorderStyle Border.unicodeRounded $ drawTaskName <=> Border.hBorder
  | otherwise  = Core.withBorderStyle (Border.borderStyleFromChar ' ') $ drawTaskName <=> Border.hBorder

  where

  drawTaskName :: Widget n
  drawTaskName =
      Core.vLimitPercent 15
    $ Core.padTopBottom 1
    $ Core.vBox
      [ Core.hCenter (Core.txt (taskName ^. nameL))
      ]

taskDescWidget :: Maybe TaskDesc -> Widget n
taskDescWidget (Just desc) = Core.vCenter $ Core.txtWrap (desc ^. descL)
taskDescWidget Nothing     = Core.vCenter $ Core.txt ""

startTimeWidget :: Time -> Widget n
startTimeWidget =
    Core.padLeft (Pad 1)
  . Core.txt
  . ("started on: " <>)
  . showTime

endTimeWidget :: Widget n
endTimeWidget =
    Core.padRight (Pad 1)
  $ Core.txt (Text.snoc "ongoing: " $ head clockAnimationStates)

taskTagsWidget :: Bool -> [TaskTag] -> Widget n
taskTagsWidget drawBorder tags
  | drawBorder = Core.withBorderStyle Border.ascii (Border.hBorder <=> drawTaskTags)
  | otherwise  = Core.withBorderStyle (Border.borderStyleFromChar ' ') (Border.hBorder <=> drawTaskTags)

  where

    drawTaskTags :: Widget n
    drawTaskTags = Core.padTopBottom 1 $ Core.txtWrap $ showTags tags
