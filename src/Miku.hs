module Miku (run) where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.BChan qualified as BChan
import Brick.Main    (App(..), neverShowCursor, customMain)
import Brick.Util    (fg)

import Control.Concurrent (threadDelay, forkIO)
import Control.Lens ((%~))

import Data.Map qualified as Map

import Graphics.Vty qualified as Vty

import Miku.UI.State (AppState(AppState), defState, keyMapL, Name, Tick(Tick))
import Miku.UI.Mode.CurrentLog (CurrentLogState)

import Miku.UI (drawUI, handleEvent)

import Relude

uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg Vty.red) []

app :: App AppState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const uiAttrMap
          }


run :: IO ()
run = do
  s' <- defState @CurrentLogState

  chan   <- BChan.newBChan 10

  void $ forkIO $ forever $ do
    BChan.writeBChan chan Tick
    threadDelay 100000

  let buildVty = Vty.mkVty Vty.defaultConfig
      initState = s'

  initialVty <- buildVty



  void $ customMain initialVty buildVty (Just chan) app $ AppState initState
