{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Miku.UI.State
  ( Action
  , AppState(AppState)
  , execAction
  , IsMode(..)
  , KeyMap
  , Keys
  , Mode(..)
  , Name
  , SMode(..)
  , Tick
  ) where

import Brick.Main qualified as Brick
import Brick.Types (Widget, BrickEvent, EventM, Next)
import Control.Lens (Lens', (^.), (.~))
import Data.Map qualified as Map

import Relude

type Name = ()
type Tick = ()

data AppState where
  AppState :: forall a. (IsMode a) => SMode a -> ModeState a -> AppState

class IsMode (a :: Mode) where
  type ModeState a :: Type
  defState         :: SMode a -> IO (ModeState a)
  drawState        :: SMode a -> ModeState a -> [Widget n]
  handleEventState :: SMode a -> ModeState a -> BrickEvent Name Tick -> EventM Name (Next AppState)

  keyMapL          :: SMode a -> Lens' (ModeState a) (KeyMap (ModeState a))
  prevKeysL        :: SMode a -> Lens' (ModeState a) Keys


type KeyMap a = Map Keys (Action a)
type Action a = a -> EventM Name (Next AppState)
type Keys     = [Char]

data Mode = WelcomeMode
          | CurrentStatusMode
          | CurrentLogMode

data SMode (mode :: Mode) where
  SWelcomeMode       :: SMode 'WelcomeMode
  SCurrentStatusMode :: SMode 'CurrentStatusMode
  SCurrentLogMode    :: SMode 'CurrentLogMode

execAction :: IsMode a => SMode a -> Action (ModeState a)
execAction mode mstate =
  case Map.lookup (mstate ^. prevKeysL mode) (mstate ^. keyMapL mode) of
    Just action -> action (mstate & prevKeysL mode .~ [])
    Nothing     -> Brick.continue $ AppState mode mstate
