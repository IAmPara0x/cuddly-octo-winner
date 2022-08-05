{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Miku.UI.State
  ( Action
  , AppState(AppState)
  , eventKey
  , execAction
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick
  ) where

import Brick.Main qualified as Brick
import Brick.Types (Widget, BrickEvent(VtyEvent), EventM, Next)
import Control.Lens (Lens', (^.), (.~))
import Data.Map qualified as Map

import Graphics.Vty qualified as Vty

import Relude

type Name = ()
type Tick = ()

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


eventKey :: BrickEvent Name Tick -> Maybe Vty.Key
eventKey (VtyEvent (Vty.EvKey key _)) = Just key
eventKey _                            = Nothing
