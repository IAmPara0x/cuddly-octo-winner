{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Miku.UI.State
  ( Action
  , AppState(AppState)
  , execAction
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick(Tick)
  ) where

import Brick.Main qualified as Brick
import Brick.Types (Widget, BrickEvent(VtyEvent), EventM, Next)
import Control.Lens (Lens', (^.), (.~), (<>~))
import Data.Map qualified as Map

import Graphics.Vty (Key(KChar, KEsc))
import Graphics.Vty qualified as Vty


import Relude

type Name = ()
data Tick = Tick

data AppState where
  AppState :: forall a. (IsMode a) => a -> AppState

class IsMode (a :: Type) where
  defState         :: IO a
  drawState        :: a -> [Widget n]
  handleEventState :: a -> BrickEvent Name Tick -> EventM Name (Next AppState)

  keyMapL          :: Lens' a (KeyMap a)
  prevKeysL        :: Lens' a Keys


type KeyMap a = Map Keys (Action a)
type Action a = a -> EventM Name (Next AppState)
type Keys     = [Char]

execAction :: forall a. IsMode a => Action a
execAction mstate =
  case Map.lookup (mstate ^. prevKeysL) (mstate ^. keyMapL) of
    Just action -> action (mstate & prevKeysL .~ [])
    Nothing     -> Brick.continue $ AppState mstate


handleAnyStateEvent :: IsMode a => a -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleAnyStateEvent modestate (VtyEvent (Vty.EvKey key [])) =
  case key of
    KEsc         -> execAction (modestate & prevKeysL .~ [])
    (KChar '\t') -> execAction (modestate & prevKeysL <>~ "<tab>")
    (KChar ' ')  -> execAction (modestate & prevKeysL <>~ "<spc>")
    (KChar c)    -> execAction (modestate & prevKeysL <>~ [c])
    _            -> Brick.continue $ AppState modestate
handleAnyStateEvent modestate _               = Brick.continue $ AppState modestate
