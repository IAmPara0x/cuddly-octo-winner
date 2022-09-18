module Miku.Mode
  ( Action
  , AppState (AppState)
  , DrawMode
  , GlobalConfig (..)
  , GlobalState (..)
  , IsMode (..)
  , KeyMap (..)
  , Keys
  , changeEditingMode
  , gcClearKeysTimeL
  , gcMaxTickCounterL
  , gcPathL
  , getKeyMap
  , gsEditingModeL
  , gsKeyMapL
  , gsKeysTickCounterL
  , gsModeConfL
  , gsModeL
  , gsModeStateL
  , gsPrevKeysL
  , gsStatusLineL
  , gsTickCounterL
  , initGlobalState
  , insertModeMapL
  , modifyAppState
  , normalModeMapL
  ) where

import Brick.Types          (BrickEvent, EventM, Next, Widget)
import Control.Lens         (Lens, lens, makeLenses, (.~))
import Data.Default         (Default (def))

import Miku.Editing         (EditingMode (..), SEditingMode (SInsert, SNormal))
import Miku.Resource        (Res, Tick)

import Miku.Draw.StatusLine (StatusLine (StatusLine))
import Miku.Draw.StatusLine qualified as StatusLine

import Relude

data GlobalConfig
  = GlobalConfig
      { _gcPathL           :: FilePath
      , _gcMaxTickCounterL :: Int
      , _gcClearKeysTimeL  :: Int
      }

instance Default GlobalConfig where
  def = GlobalConfig { _gcPathL           = "/home/iamparadox/.miku/"
                     , _gcMaxTickCounterL = 100
                     , _gcClearKeysTimeL  = 5
                     }

type GlobalState :: EditingMode -> Type -> Type
data GlobalState emode mode
  = IsMode mode => GlobalState
      { _gsKeysTickCounterL :: Int
      , _gsTickCounterL     :: Int
      , _gsModeStateL       :: ModeState mode
      , _gsKeyMapL          :: KeyMap mode
      , _gsModeConfL        :: ModeConf mode
      , _gsPrevKeysL        :: Keys
      , _gsEditingModeL     :: SEditingMode emode
      , _gsStatusLineL      :: StatusLine emode
      }

data KeyMap mode
  = KeyMap
      { _insertModeMapL :: Map Keys (Action 'Insert mode)
      , _normalModeMapL :: Map Keys (Action 'Normal mode)
      }

instance Semigroup (KeyMap mode) where
  KeyMap i1 n1 <> KeyMap i2 n2 = KeyMap (i1 <> i2) (n1 <> n2)

instance Monoid (KeyMap mode) where
  mempty = KeyMap mempty mempty

type Keys = [Char]
type Action emode mode = StateT (GlobalState emode mode) (EventM Res) (Next AppState)
type DrawMode emode mode = Reader (GlobalState emode mode) (Widget Res)

data AppState
  = forall emode mode. (IsMode mode) => AppState (GlobalState emode mode)

class IsMode (mode :: Type) where
  type ModeState mode :: Type
  type ModeConf  mode :: Type

  drawstate        :: DrawMode emode mode
  handleEventState :: BrickEvent Res Tick -> Action emode mode

makeLenses ''KeyMap
makeLenses ''GlobalState
makeLenses ''GlobalConfig

getKeyMap :: GlobalState emode mode -> Map Keys (Action emode mode)
getKeyMap gstate = case _gsEditingModeL gstate of
  SNormal -> _normalModeMapL $ _gsKeyMapL gstate
  SInsert -> _insertModeMapL $ _gsKeyMapL gstate

gsModeL
  :: IsMode b
  => Lens
       (GlobalState 'Normal a)
       (GlobalState 'Normal b)
       (ModeState a, ModeConf a, KeyMap a)
       (ModeState b, ModeConf b, KeyMap b)
gsModeL = lens getter setter
 where
  getter gstate = (_gsModeStateL gstate, _gsModeConfL gstate, _gsKeyMapL gstate)
  setter gstate (mstate, mconf, keymap) = GlobalState
    { _gsModeStateL       = mstate
    , _gsKeyMapL          = keymap
    , _gsModeConfL        = mconf
    , _gsTickCounterL     = _gsTickCounterL gstate
    , _gsKeysTickCounterL = _gsKeysTickCounterL gstate
    , _gsPrevKeysL        = []
    , _gsEditingModeL     = _gsEditingModeL gstate
    , _gsStatusLineL      = StatusLine (_gsEditingModeL gstate) []
    }

modifyAppState :: (forall e m . GlobalState e m -> GlobalState e m) -> AppState -> AppState
modifyAppState f (AppState s) = AppState (f s)

initGlobalState :: IsMode m => ModeState m -> ModeConf m -> KeyMap m -> GlobalState 'Normal m
initGlobalState mstate mconf keymap = GlobalState { _gsKeysTickCounterL = 0
                                                  , _gsTickCounterL     = 0
                                                  , _gsModeStateL       = mstate
                                                  , _gsModeConfL        = mconf
                                                  , _gsKeyMapL          = keymap
                                                  , _gsPrevKeysL        = []
                                                  , _gsEditingModeL     = SNormal
                                                  , _gsStatusLineL      = def
                                                  }

changeEditingMode :: IsMode a => SEditingMode e2 -> GlobalState e1 a -> GlobalState e2 a
changeEditingMode e GlobalState {..} = GlobalState
  { _gsKeysTickCounterL = _gsKeysTickCounterL
  , _gsTickCounterL     = _gsTickCounterL
  , _gsModeStateL       = _gsModeStateL
  , _gsKeyMapL          = _gsKeyMapL
  , _gsPrevKeysL        = _gsPrevKeysL
  , _gsEditingModeL     = e
  , _gsStatusLineL      = _gsStatusLineL & StatusLine.slEditingModeL .~ e
  , _gsModeConfL        = _gsModeConfL
  }
