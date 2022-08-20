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
  AppState :: forall a. (IsMode a) => a -> AppState

class IsMode (a :: Type) where
  defState         :: IO a
  drawState        :: a -> [Widget n]
  handleEventState :: a -> BrickEvent Name Tick -> EventM Name (Next AppState)

  keyMapL          :: Lens' a (KeyMap a)
  prevKeysL        :: Lens' a Keys

type KeyMap a = Map Keys (Action a)
type Keys     = [Char]
type Action a = ReaderT a (EventM Name) (Next a)

execAction :: forall a. IsMode a => Action a
execAction = do
  mstate <- ask

  case Map.lookup (mstate ^. prevKeysL) (mstate ^. keyMapL) of
      Just action -> lift $ fmap (prevKeysL .~ []) <$> runReaderT action mstate
      Nothing     -> lift $ Brick.continue mstate


runAction :: forall a. IsMode a => Action a -> a -> EventM Name (Next AppState)
runAction action = fmap (fmap AppState) . runReaderT action

handleAnyStateEvent :: IsMode a => a -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleAnyStateEvent modestate (VtyEvent (Vty.EvKey key [])) =
  case key of
    KEsc         -> runAction execAction (modestate & prevKeysL .~ [])
    (KChar '\t') -> runAction execAction (modestate & prevKeysL <>~ "<tab>")
    (KChar ' ')  -> runAction execAction (modestate & prevKeysL <>~ "<spc>")
    (KChar c)    -> runAction execAction (modestate & prevKeysL <>~ [c])
    _            -> Brick.continue $ AppState modestate
handleAnyStateEvent modestate _               = Brick.continue $ AppState modestate
