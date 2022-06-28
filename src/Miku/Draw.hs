{-# LANGUAGE GADTs #-}
module Miku.Draw (drawUI, handleEvent) where

import Graphics.Vty qualified as V

import Brick.Main (halt)

import Brick.Types
  ( BrickEvent(VtyEvent)
  , EventM
  , Next
  , Widget
  )

import Miku.UI
  ( UI(..)
  , Layout(Main)
  , ResourceName
  , Tick(Tick)
  )
import Miku.Draw.MainLayout (drawUIMain, handleEventMain)

drawUI :: UI layout -> [Widget ResourceName]
drawUI ui@(MainUI _) = drawUIMain ui

handleEvent :: UI layout -> BrickEvent ResourceName Tick -> EventM ResourceName (Next (UI layout))
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui@(MainUI _) e                          = handleEventMain ui e
