module Miku.UI.Draw.CurrentTask
  ( CurrentTask(..)
  , CurrentTaskName(..)
  , CurrentTaskTags(..)
  , CurrentTaskDesc(..)
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
  ( Task(..)
  , TaskName(TaskName)
  , TaskTag
  , TaskDesc
  , showTags
  , descL
  , nameL
  )
import Miku.Types.Time (Time, showTime)
import Miku.UI.Draw (Border(..), Drawable(..))

import Prelude ((!!))


data CurrentTask = CurrentTask Int Task
                 | NoCurrentTask Text

instance Drawable CurrentTask where
  draw Hidden (NoCurrentTask text)  = Core.withBorderStyle (Border.borderStyleFromChar ' ')
                                      $ Border.border
                                      $ noOngoinTaskWidget text
  draw Rounded (NoCurrentTask text) = Core.withBorderStyle Border.unicodeRounded
                                      $ Border.border
                                      $ noOngoinTaskWidget text
  draw Hidden (CurrentTask idx task)    = Core.withBorderStyle (Border.borderStyleFromChar ' ')
                                      $ Border.border
                                      $ ongoinTaskWidget Hidden idx task
  draw Rounded (CurrentTask idx task)   = Core.withBorderStyle Border.unicodeRounded
                                      $ Border.border
                                      $ ongoinTaskWidget Rounded idx task



newtype CurrentTaskName = CurrentTaskName TaskName

instance Drawable CurrentTaskName where
  draw border (CurrentTaskName taskName)
    | border == Hidden  = Core.withBorderStyle (Border.borderStyleFromChar ' ') $ drawTaskName <=> Border.hBorder
    | otherwise         = Core.withBorderStyle Border.unicodeRounded $ drawTaskName <=> Border.hBorder
    where
      drawTaskName :: Widget n
      drawTaskName =
          Core.vLimitPercent 15
        $ Core.padTopBottom 1
        $ Core.vBox
          [ Core.hCenter (Core.txt (taskName ^. nameL))
          ]

noOngoinTaskWidget :: Text -> Widget n
noOngoinTaskWidget = Core.hLimitPercent 50
                   . Core.center
                   . Core.txt

ongoinTaskWidget :: Border -> Int -> Task -> Widget n
ongoinTaskWidget border idx Task{..} =
    Core.hLimitPercent 50
  $ Core.vBox
    [ draw border (coerce @_ @CurrentTaskName _taskNameL)
    , Core.padTop (Pad 1)
      $ Core.vLimit 2
      $ Core.hBox [ draw border (coerce @_ @StartTime _taskStartL)
                  , Core.fill ' '
                  , draw border (EndTime idx _taskEndL)
                  ]
    , Core.padLeft (Pad 4) $ Core.padTopBottom 1
       $ draw border (coerce @_ @CurrentTaskDesc _taskDescL)
    , draw border (coerce @_ @CurrentTaskTags _taskTagsL)

    ]

newtype CurrentTaskDesc = CurrentTaskDesc (Maybe TaskDesc)

instance Drawable CurrentTaskDesc where
  draw _ (CurrentTaskDesc (Just desc)) = Core.vCenter $ Core.txtWrap (desc ^. descL)
  draw _ (CurrentTaskDesc Nothing)     = Core.vCenter $ Core.txt ""


newtype StartTime = StartTime Time

instance Drawable StartTime where
  draw _ = Core.padLeft (Pad 1)
         . Core.txt
         . ("started on: " <>)
         . showTime
         . coerce

data EndTime = EndTime Int (Maybe Time)

instance Drawable EndTime where
  draw _ (EndTime idx _) = Core.padRight (Pad 1)
                         $ Core.txt (Text.snoc "ongoing: " $ clockAnimationStates !! idx) -- TODO: remove (!!).

newtype CurrentTaskTags = CurrentTaskTags [TaskTag]

instance Drawable CurrentTaskTags where
  draw border (CurrentTaskTags tags)
    | border == Hidden = Core.withBorderStyle (Border.borderStyleFromChar ' ') (Border.hBorder <=> drawTaskTags)
    | otherwise        = Core.withBorderStyle Border.ascii (Border.hBorder <=> drawTaskTags)

    where

      drawTaskTags :: Widget n
      drawTaskTags = Core.padTopBottom 1 $ Core.txtWrap $ showTags tags

clockAnimationStates :: [Char]
clockAnimationStates = '◴' : ['◷','◶','◵']
