module Miku (run) where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main    (App(..), neverShowCursor, defaultMain)
import Brick.Util    (fg)

import Graphics.Vty qualified as V

import Miku.UI.State (AppState(WState), Name, Tick)
import Miku.UI.State.Welcome (WelcomeConfig(WelcomeConfig), WelcomeState(WelcomeState))
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
run = void $ defaultMain app $ WState (WelcomeConfig "path") (WelcomeState "Moshi Moshi Moshi")
