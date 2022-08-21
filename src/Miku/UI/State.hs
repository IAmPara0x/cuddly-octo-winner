{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Miku.UI.State
  ( Action
  , AppState(AppState)
  , runAction
  , execAction
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick(Tick)
  , DrawMode
  ) where

import Brick.Main qualified as Brick
import Brick.Types (Widget, BrickEvent(VtyEvent), EventM, Next)
import Control.Lens (Lens', (^.), (.~), (<>~))
-- import Control.Monad.Trans.Reader (Reade)
import Data.Map qualified as Map

import Graphics.Vty (Key(KChar, KEsc))
import Graphics.Vty qualified as Vty


import Relude

type Name = ()
data Tick = Tick

data AppState where
  AppState :: forall a. (IsMode a) => Proxy a -> ModeState a -> AppState

-- TODO: use some kind of constraint on (ModeState a) to achieve behaviour of "data inheritance".
class IsMode (a :: Type) where
  type ModeState a :: Type

  defState         :: IO (ModeState a)
  drawState        :: DrawMode a
  handleEventState :: ModeState a -> BrickEvent Name Tick -> EventM Name (Next AppState)

  -- TODO: Find a better way for something like "data inheritance".
  keyMapL          :: Lens' (ModeState a) (KeyMap a)
  prevKeysL        :: Lens' (ModeState a) Keys

type DrawMode a = Reader (ModeState a) [Widget Name]

type KeyMap a = Map Keys (Action a)
type Keys     = [Char]
type Action a = ReaderT (ModeState a) (EventM Name) (Next (ModeState a))

execAction :: forall a. IsMode a => Action a
execAction = do
  mstate <- ask

  case Map.lookup (mstate ^. prevKeysL @a) (mstate ^. keyMapL @a) of
      Just action -> lift $ fmap (prevKeysL @a .~ []) <$> runReaderT action mstate
      Nothing     -> lift $ Brick.continue mstate


runAction :: forall a. IsMode a => Action a -> ModeState a -> EventM Name (Next AppState)
runAction action = fmap (fmap (AppState @a Proxy)) . runReaderT action

handleAnyStateEvent :: forall a. IsMode a => ModeState a -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleAnyStateEvent modestate (VtyEvent (Vty.EvKey key [])) =
  case key of
    KEsc         -> runAction @a (execAction @a) (modestate & prevKeysL @a .~ [])
    (KChar '\t') -> runAction @a (execAction @a) (modestate & prevKeysL @a <>~ "<tab>")
    (KChar ' ')  -> runAction @a (execAction @a) (modestate & prevKeysL @a <>~ "<spc>")
    (KChar c)    -> runAction @a (execAction @a) (modestate & prevKeysL @a <>~ [c])
    _            -> Brick.continue $ AppState @a Proxy modestate
handleAnyStateEvent modestate _               = Brick.continue $ AppState @a Proxy modestate
