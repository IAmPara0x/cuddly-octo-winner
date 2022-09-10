module Miku
  ( run
  ) where

import Brick.AttrMap        (AttrMap, attrMap)
import Brick.BChan          qualified as BChan
import Brick.Main           (App (..), customMain, neverShowCursor)
import Brick.Util           (fg)

import Control.Concurrent   (forkIO, threadDelay)
import Data.Default         (def)
import Graphics.Vty         qualified as Vty

import Miku.Editing         (SEditingMode (SNormal))
import Miku.Mode            (AppState (AppState), GlobalState (..), Name,
                             Tick (Tick), defState)
import Miku.Mode.CurrentLog (CurrentLog)

import Miku.UI              (drawUI, handleEvent)

import Relude

uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg Vty.red) [("todo", fg Vty.white), ("current", fg Vty.blue)]

app :: App AppState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const uiAttrMap
          }


run :: IO ()
run = do
  (s, k) <- defState @CurrentLog

  chan   <- BChan.newBChan 10

  void $ forkIO $ forever $ do
    BChan.writeBChan chan Tick
    threadDelay 100000

  let buildVty = Vty.mkVty Vty.defaultConfig
      initState = GlobalState { _gsConfigL = def
                              , _gsKeysTickCounterL = 0
                              , _gsTickCounterL = 0
                              , _gsModeStateL = s
                              , _gsKeyMapL = k
                              , _gsPrevKeysL = []
                              , _gsEditingModeL = SNormal
                              }

  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just chan) app $ AppState initState
