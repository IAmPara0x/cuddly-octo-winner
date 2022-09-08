module Miku.Mode
  ( Action
  , AppState (AppState)
  , DrawMode
  , GlobalConfig (..)
  , GlobalState (..)
  , IsMode (..)
  , KeyMap
  , Keys
  , Name
  , Tick (Tick)
  , continueAction
  , gcMaxTickCounterL
  , gcPathL
  , gsChangeModeL
  , gsConfigL
  , gsEditingModeL
  , gsKeyMapL
  , gsModeStateL
  , gsPrevKeysL
  , gsTickCounterL
  , haltAction
  , handleAnyStateEvent
  ) where

import Brick.Main   qualified as Brick
import Brick.Types  (BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget)
import Control.Lens (Lens, Lens', _1, _2, lens, makeLenses, (%~), (+~), (.~),
                     (<>~), (^.))
import Data.Default (Default (def))
import Data.Map     qualified as Map

import Graphics.Vty (Key (KChar, KEsc))
import Graphics.Vty qualified as Vty

import Miku.Editing (EMode (..))


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
              , _gsEditingModeL     :: EMode
              }

type KeyMap a   = Map Keys (Action a)
type Keys       = [Char]
type Action a   = StateT (GlobalState a) (EventM Name) (Next AppState)
type DrawMode a = Reader (GlobalState a) [Widget Name]

data AppState where
  AppState :: forall a. (IsMode a) => GlobalState a -> AppState

class IsMode (a :: Type) where
  type ModeState a :: Type

  defState         :: IO (ModeState a, KeyMap a)
  drawState        :: DrawMode a
  handleEventState :: BrickEvent Name Tick -> Action a

makeLenses ''GlobalState
makeLenses ''GlobalConfig

gsChangeModeL :: IsMode b =>
  Lens (GlobalState a)
       (GlobalState b)
       (ModeState a, KeyMap a)
       (ModeState b, KeyMap b)
gsChangeModeL = lens getter setter
  where
    getter gstate                   = (gstate ^. gsModeStateL, gstate ^. gsKeyMapL)
    setter gstate (mstate, keymap)  = GlobalState { _gsModeStateL       = mstate
                                                  , _gsKeyMapL          = keymap
                                                  , _gsConfigL          = _gsConfigL gstate
                                                  , _gsTickCounterL     = _gsTickCounterL gstate
                                                  , _gsKeysTickCounterL = _gsKeysTickCounterL gstate
                                                  , _gsPrevKeysL        = []
                                                  , _gsEditingModeL     = Normal
                                                  }


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
    getter (AppState s) = s ^. gsPrevKeysL
    setter (AppState s) keys = AppState $ s & gsPrevKeysL .~ keys


handleAnyStateEvent :: forall a. IsMode a => BrickEvent Name Tick -> Action a
handleAnyStateEvent (AppEvent Tick) = tickAction
handleAnyStateEvent (VtyEvent (Vty.EvKey key [])) =
  case key of
    KEsc         -> modify ((gsPrevKeysL .~ []) . (gsKeysTickCounterL .~ 0)) >> continueAction
    (KChar '\t') -> actionWithKeys "<tab>"
    (KChar ' ')  -> actionWithKeys "<spc>"
    (KChar c)    -> actionWithKeys [c]
    _            -> continueAction
handleAnyStateEvent _               = continueAction


actionWithKeys :: IsMode a => Keys -> Action a
actionWithKeys keys = do
  modify ((gsPrevKeysL <>~ keys) . (gsKeysTickCounterL .~ 0))
  gstate <- get

  case Map.lookup (gstate ^. gsPrevKeysL) (gstate ^. gsKeyMapL) of
    Just action -> fmap (clearKeysL .~ []) <$> action
    Nothing     -> continueAction

continueAction :: IsMode a => Action a
continueAction = get >>= lift . Brick.continue . AppState

haltAction :: IsMode a => Action a
haltAction = get >>= lift . Brick.halt . AppState

tickAction :: IsMode a => Action a
tickAction = fmap (clearPrevKeys . updateTickCounter) <$> continueAction

-- | Helpers

updateTickCounter :: AppState -> AppState
updateTickCounter (AppState gstate) = AppState $
  gstate & gsTickCounterL .~ uncurry mod
              (gstate ^. gsTickL & _1 +~ 1)
          & gsKeysTickCounterL .~ uncurry mod
              (gstate ^. gsKeysTickL & _1 +~ 1 & _2 .~ gstate ^. gsTickL . _2)

clearPrevKeys :: AppState -> AppState
clearPrevKeys (AppState gstate) =
    AppState
  $ gstate & gsPrevKeysL %~
      bool id (const []) (uncurry rem (gstate ^. gsKeysTickL) == 0)

