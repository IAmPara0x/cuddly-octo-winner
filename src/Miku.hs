module Miku (run) where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main    (App(..), neverShowCursor, defaultMain)
import Brick.Util    (fg)

import Control.Lens ((%~))

import Data.Map qualified as Map

import Graphics.Vty qualified as V

import Miku.UI.State (AppState(AppState), defState, keyMapL, Name, SMode(SWelcomeMode, SCurrentLogMode), Tick)
import Miku.UI.Mode.Welcome ()
import Miku.UI.Mode.CurrentLog (toCurrentLogMode)

import Miku.UI (drawUI, handleEvent)

import Relude

uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg V.red) []

app :: App AppState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const uiAttrMap
          }


run :: IO ()
run = do
  s' <- defState SWelcomeMode

  let s = s' & keyMapL SWelcomeMode %~ Map.insert " cl" (toCurrentLogMode SWelcomeMode)

  void $ defaultMain app $ AppState SWelcomeMode s
