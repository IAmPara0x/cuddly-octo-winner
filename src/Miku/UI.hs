{-# LANGUAGE GADTs #-}
module Miku.UI
  ( drawUI
  , handleEvent
  ) where

import Brick.Types          (BrickEvent, EventM, Next, Widget)
import Brick.Widgets.Core   qualified as Core
import Miku.Draw.StatusLine (drawStatusLine)
import Miku.Mode            (AppState (AppState), GlobalState)
import Miku.Mode            qualified as Mode
import Miku.Resource        (Res, Tick)

import Relude

drawUI :: AppState -> [Widget Res]
drawUI (AppState (s :: GlobalState emode mode)) =
  let (Right w) = runReader (runExceptT $ Mode.drawstate @mode) s
  in  [Core.vBox [w, drawStatusLine $ Mode._gsStatusLineL s]]

handleEvent :: AppState -> BrickEvent Res Tick -> EventM Res (Next AppState)
handleEvent (AppState (s :: GlobalState emode mode)) event = do
  Right a <- evalStateT (runExceptT (Mode.handleEventState @mode event)) s
  return a
