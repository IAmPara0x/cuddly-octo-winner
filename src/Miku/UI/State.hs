{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Miku.UI.State
  ( Action
  , AppState(AppState)
  , execAction
  , handleAnyStateEvent
  , GlobalConfig(..)
  , gcPathL
  , gcMaxTickCounterL
  , GlobalState(..)
  , gsConfigL
  , gsTickCounterL
  , gsModeStateL
  , gsKeyMapL
  , gsPrevKeysL
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick(Tick)
  , DrawMode
  ) where

import Brick.Main qualified as Brick
import Brick.Types (Widget, BrickEvent(VtyEvent, AppEvent), EventM, Next)
import Control.Lens ((^.), (.~), (<>~), makeLenses, Lens', lens, _1, _2, (+~), (%~))
import Data.Default (Default(def))
import Data.Map qualified as Map

import Graphics.Vty (Key(KChar, KEsc))
import Graphics.Vty qualified as Vty


import Relude

type Name = ()
data Tick = Tick

data GlobalConfig =
  GlobalConfig { _gcPathL           :: FilePath
               , _gcMaxTickCounterL :: Int
               , _gcClearKeysTimeL  :: Int
               }

instance Default GlobalConfig where
  def = GlobalConfig { _gcPathL = "/home/iamparadox/.miku/"
                     , _gcMaxTickCounterL = 100
                     , _gcClearKeysTimeL = 5
                     }

data GlobalState a = IsMode a =>
  GlobalState { _gsConfigL          :: GlobalConfig
              , _gsKeysTickCounterL :: Int
              , _gsTickCounterL     :: Int
              , _gsModeStateL       :: ModeState a
              , _gsKeyMapL          :: KeyMap a
              , _gsPrevKeysL        :: Keys
              }

type KeyMap a   = Map Keys (Action a)
type Keys       = [Char]
type Action a   = ReaderT (GlobalState a) (EventM Name) (Next AppState)
type DrawMode a = Reader (GlobalState a) [Widget Name]

data AppState where
  AppState :: forall a. (IsMode a) => Proxy a -> GlobalState a -> AppState

class IsMode (a :: Type) where
  type ModeState a :: Type

  defState         :: IO (ModeState a)
  drawState        :: DrawMode a
  handleEventState :: GlobalState a -> BrickEvent Name Tick -> EventM Name (Next AppState)

makeLenses ''GlobalState
makeLenses ''GlobalConfig

-- gsChangeModeL :: Lens

gsTickL :: Lens' (GlobalState a) (Int, Int)
gsTickL  = lens getter setter
  where
    getter gstate
        = (gstate ^. gsTickCounterL, gstate ^. gsConfigL . gcMaxTickCounterL)
    setter gstate (val, conf)
        = gstate  & gsTickCounterL .~ val
                  & gsConfigL . gcMaxTickCounterL .~ conf

gsKeysTickL :: Lens' (GlobalState a) (Int, Int)
gsKeysTickL  = lens getter setter
  where
    getter gstate
        = (gstate ^. gsKeysTickCounterL, gstate ^. gsConfigL . gcClearKeysTimeL)
    setter gstate (val, conf)
        = gstate & gsKeysTickCounterL .~ val
                  & gsConfigL . gcClearKeysTimeL .~ conf

clearKeysL :: Lens' AppState Keys
clearKeysL = lens getter setter
  where
    getter (AppState _ s) = s ^. gsPrevKeysL
    setter (AppState p s) keys = AppState p $ s & gsPrevKeysL .~ keys

execAction :: forall a. IsMode a => Action a
execAction = do
  mstate <- ask

  case Map.lookup (mstate ^. gsPrevKeysL) (mstate ^. gsKeyMapL) of
    Just action -> fmap (clearKeysL .~ []) <$> action
    Nothing     -> lift $ Brick.continue $ AppState Proxy mstate

handleAnyStateEvent :: forall a. IsMode a
                    => GlobalState a
                    -> BrickEvent Name Tick
                    -> EventM Name (Next AppState)
handleAnyStateEvent gstate (AppEvent Tick)               =
  Brick.continue $ AppState Proxy $ updateTickCounter gstate
                                  & id %~ clearPrevKeys
handleAnyStateEvent gstate (VtyEvent (Vty.EvKey key [])) =
  case key of
    KEsc         -> runReaderT execAction (gstate & gsPrevKeysL .~ [] & gsKeysTickCounterL .~ 0)
    (KChar '\t') -> runReaderT execAction (gstate & gsPrevKeysL <>~ "<tab>" & gsKeysTickCounterL .~ 0)
    (KChar ' ')  -> runReaderT execAction (gstate & gsPrevKeysL <>~ "<spc>" & gsKeysTickCounterL .~ 0)
    (KChar c)    -> runReaderT execAction (gstate & gsPrevKeysL <>~ [c] & gsKeysTickCounterL .~ 0)
    _            -> Brick.continue $ AppState Proxy gstate
handleAnyStateEvent gstate _                             =
  Brick.continue $ AppState Proxy gstate

updateTickCounter :: GlobalState a -> GlobalState a
updateTickCounter gstate =
  gstate & gsTickCounterL .~ uncurry mod
              (gstate ^. gsTickL & _1 +~ 1)
          & gsKeysTickCounterL .~ uncurry mod
              (gstate ^. gsKeysTickL & _1 +~ 1 & _2 .~ gstate ^. gsTickL . _2)


clearPrevKeys :: GlobalState a -> GlobalState a
clearPrevKeys gstate
  = gstate & gsPrevKeysL %~
      bool id (const []) (uncurry rem (gstate ^. gsKeysTickL) == 0)
