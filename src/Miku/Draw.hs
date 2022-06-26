{-# LANGUAGE GADTs #-}
module Miku.Draw (drawUI, handleEvent) where

import Graphics.Vty qualified as V

import Brick.Main (continue, halt)

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
  )
import Miku.Draw.Main (drawMainUI)

drawUI :: UI mode -> [Widget ResourceName]
drawUI ui@(MainUI _) = drawMainUI ui

-- TODO: Handle events in seperate module.

handleEvent :: UI mode -> BrickEvent ResourceName n -> EventM ResourceName (Next (UI mode))
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui _                                     = continue ui
