module Miku (run) where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main    (App(..), neverShowCursor, defaultMain)
import Brick.Util    (fg)

import Graphics.Vty qualified as V

import Relude

import Miku.Types.Log
import Miku.UI   (UI(MainUI), MainState(MainState), ResourceName)
import Miku.Draw (drawUI, handleEvent)


uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg V.red) []

app :: App (UI mode) e ResourceName
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const uiAttrMap
   
          }
--
run :: IO ()
run = do
  log <- readLog "dailyLog.md"
  time   <- currentTime
  void $ defaultMain app (MainUI (MainState log time))
