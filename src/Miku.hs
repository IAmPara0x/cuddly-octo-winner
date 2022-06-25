module Miku (run) where

import Brick.Main (App(..), neverShowCursor, defaultMain)

import Relude

import Miku.Types.Log
import Miku.UI.Log

app :: App UI e ResourceName
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
  void $ defaultMain app (UI Main log)
  
  
