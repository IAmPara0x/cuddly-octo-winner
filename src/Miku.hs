module Miku (run) where

import Brick.AttrMap (AttrMap, attrMap, applyAttrMappings)
import Brick.BChan   (newBChan, writeBChan)
import Brick.Main    (App(..), neverShowCursor, customMain)
import Brick.Util    (fg)
import Control.Concurrent (threadDelay, forkIO)

import Graphics.Vty qualified as V

import Relude

import Miku.Types.Log
import Miku.UI
  ( UI(MainUI)
  , MainRes(..)
  , MainState(MainState)
  , Resource(MainRes)
  , Tick(Tick)
  )
import Miku.Draw (drawUI, handleEvent)
import Graphics.Vty.Image (char)

uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg V.white) []

app :: App (UI layout) Tick (Resource layout)
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const uiAttrMap
          }

run :: IO ()
run = do
  log    <- readLog "dailyLog.md"
  time   <- getCurrentTime

  chan   <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000

  let buildVty = V.mkVty V.defaultConfig
      mainstate = MainState log time 0 (MainRes NotCompletedGoals)

  initialVty <- buildVty


  void $ customMain initialVty buildVty (Just chan) app (MainUI mainstate)
