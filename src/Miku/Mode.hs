module Miku.Mode
  ( Action
  , AppState (AppState)
  , DrawMode
  , GlobalConfig (..)
  , GlobalState (..)
  , IsMode (..)
  , KeyMap (..)
  , Keys
  , Name
  , Tick (Tick)
  , clearKeysL
  , gcClearKeysTimeL
  , gcMaxTickCounterL
  , gcPathL
  , getKeyMap
  , gsChangeModeL
  , gsConfigL
  , gsEditingModeL
  , gsKeyMapL
  , gsKeysTickCounterL
  , gsKeysTickL
  , gsModeStateL
  , gsPrevKeysL
  , gsTickCounterL
  , gsTickL
  , insertModeMapL
  , normalModeMapL
  ) where

import Brick.Types  (BrickEvent, EventM, Next, Widget)
import Control.Lens (Lens, Lens', lens, makeLenses, (.~), (^.))
import Data.Default (Default (def))


import Miku.Editing (EditingMode (..), SEditingMode (SInsert, SNormal))


import Relude

type Name = ()

data Tick
  = Tick

data GlobalConfig
  = GlobalConfig
      { _gcPathL           :: FilePath
      , _gcMaxTickCounterL :: Int
      , _gcClearKeysTimeL  :: Int
      }

instance Default GlobalConfig where
  def = GlobalConfig { _gcPathL = "/home/iamparadox/.miku/"
                     , _gcMaxTickCounterL = 100
                     , _gcClearKeysTimeL = 5
                     }

type GlobalState :: EditingMode -> Type -> Type
data GlobalState emode mode
  = IsMode mode => GlobalState
      { _gsConfigL          :: GlobalConfig
      , _gsKeysTickCounterL :: Int
      , _gsTickCounterL     :: Int
      , _gsModeStateL       :: ModeState mode
      , _gsKeyMapL          :: KeyMap mode
      , _gsPrevKeysL        :: Keys
      , _gsEditingModeL     :: SEditingMode emode
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

type Keys                = [Char]
type Action emode mode   = StateT (GlobalState emode mode) (EventM Name) (Next AppState)
type DrawMode emode mode = Reader (GlobalState emode mode) [Widget Name]

data AppState where
  AppState :: forall (emode :: EditingMode) (mode :: Type). (IsMode mode) => GlobalState emode mode -> AppState

class IsMode (mode :: Type) where
  type ModeState mode :: Type

  defState         :: IO (ModeState mode, KeyMap mode)
  drawState        :: DrawMode emode mode
  handleEventState :: BrickEvent Name Tick -> Action emode mode

makeLenses ''KeyMap
makeLenses ''GlobalState
makeLenses ''GlobalConfig

getKeyMap :: GlobalState emode mode -> Map Keys (Action emode mode)
getKeyMap gstate = case _gsEditingModeL gstate of
                     SNormal -> _normalModeMapL $ _gsKeyMapL gstate
                     SInsert -> _insertModeMapL $ _gsKeyMapL gstate

gsChangeModeL :: IsMode b =>
  Lens (GlobalState 'Normal a)
       (GlobalState 'Normal b)
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
                                                  , _gsEditingModeL     = _gsEditingModeL gstate
                                                  }

gsTickL :: Lens' (GlobalState emode mode) (Int, Int)
gsTickL  = lens getter setter
  where
    getter gstate
        = (gstate ^. gsTickCounterL, gstate ^. gsConfigL . gcMaxTickCounterL)
    setter gstate (val, conf)
        = gstate  & gsTickCounterL .~ val
                  & gsConfigL . gcMaxTickCounterL .~ conf

gsKeysTickL :: Lens' (GlobalState emode mode) (Int, Int)
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


