module Miku.UI.Draw.CurrentTask
  ( drawCurrentTask
  )
  where

import Brick.Types (Padding (Pad), Widget)
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Relude

import Miku.Templates.Log (Task)

clockAnimationStates :: [Char]
clockAnimationStates = ['◴','◷','◶','◵','◴']

drawCurrentTask :: Bool -> Either Text Task -> Widget n
drawCurrentTask True (Left text) =
    Core.withBorderStyle Border.unicodeRounded
  $ Border.border
  $ Core.hLimitPercent 65
  $ Core.vLimitPercent 50
  $ Core.center
  $ Core.txt text
