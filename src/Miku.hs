module Miku
  ( run
  ) where

import Brick.AttrMap        (AttrMap, attrMap)
import Brick.BChan          qualified as BChan
import Brick.Main           (App (..), customMain, neverShowCursor)
import Brick.Util           (fg)

import Control.Concurrent   (forkIO, threadDelay)
import Graphics.Vty         qualified as Vty

import Miku.Mode            (AppState (AppState))
import Miku.Mode.CurrentLog qualified as CurrentLog
import Miku.Resource        (Res, Tick (Tick))

import Miku.UI              (drawUI, handleEvent)

import Relude

uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg Vty.red) [("todo", fg Vty.white), ("current", fg Vty.blue)]

app :: App AppState Tick Res
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const uiAttrMap
          }


run :: IO ()
run = do
  chan              <- BChan.newBChan 10
  (Right initState) <- runExceptT CurrentLog.initCurrentLogMode

  void $ forkIO $ forever $ do
    BChan.writeBChan chan Tick
    threadDelay 100000

  let buildVty = Vty.mkVty Vty.defaultConfig

  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just chan) app $ AppState initState
