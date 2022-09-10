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
  , insertModeMapL
  , normalModeMapL
  , toNormalMode
  ) where

import Brick.Main   qualified as Brick
import Brick.Types  (BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget)
import Control.Lens (Lens, Lens', _1, _2, lens, makeLenses, (%~), (+~), (.~),
                     (<>~), (^.))
import Data.Default (Default (def))
import Data.Map     qualified as Map

import Graphics.Vty (Key (KBackTab, KChar, KEsc))
import Graphics.Vty qualified as Vty

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

getKeyMap :: GlobalState emode mode -> Map Keys (Action emode mode)
getKeyMap gstate = case _gsEditingModeL gstate of
                    SNormal -> _normalModeMapL $ _gsKeyMapL gstate
                    SInsert -> _insertModeMapL $ _gsKeyMapL gstate

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

handleAnyStateEvent :: IsMode a => BrickEvent Name Tick -> Action emode a
handleAnyStateEvent event = do
  gstate <- get
  case gstate ^. gsEditingModeL of
    SNormal -> handleNormalStateEvent event
    SInsert -> handleInsertStateEvent event

handleNormalStateEvent :: forall a. IsMode a => BrickEvent Name Tick -> Action 'Normal a
handleNormalStateEvent (AppEvent Tick) = tickAction
handleNormalStateEvent (VtyEvent (Vty.EvKey key [])) =
  case key of
    KEsc         -> modify ((gsPrevKeysL .~ []) . (gsKeysTickCounterL .~ 0)) >> continueAction
    (KChar '\t') -> actionWithKeys "<tab>"
    (KChar ' ')  -> actionWithKeys "<spc>"
    (KChar 'i')  -> toInsertMode
    (KChar c)    -> actionWithKeys [c]
    KBackTab     -> actionWithKeys "<shift>+<tab>"
    _            -> continueAction
handleNormalStateEvent _               = continueAction

handleInsertStateEvent :: forall a. IsMode a => BrickEvent Name Tick -> Action 'Insert a
handleInsertStateEvent (AppEvent Tick)                     = tickAction
handleInsertStateEvent (VtyEvent (Vty.EvKey KEsc []))      = toNormalMode
handleInsertStateEvent (VtyEvent (Vty.EvKey (KChar c) [])) = actionWithKeys [c]
handleInsertStateEvent _                                   = continueAction

-- | Helpers

toInsertMode :: IsMode a => Action 'Normal a
toInsertMode = do
  gstate <- get
  lift $ Brick.continue (AppState $ gstate & gsEditingModeL .~ SInsert)

toNormalMode :: IsMode a => Action 'Insert a
toNormalMode = do
  gstate <- get
  lift $ Brick.continue (AppState $ gstate & gsEditingModeL .~ SNormal)

actionWithKeys :: forall a emode. IsMode a => Keys -> Action emode a
actionWithKeys keys = do
  modify ((gsPrevKeysL <>~ keys) . (gsKeysTickCounterL .~ 0))
  gstate <- get

  case Map.lookup (gstate ^. gsPrevKeysL) (getKeyMap gstate) of
    Just action -> fmap (clearKeysL .~ []) <$> action
    Nothing     -> continueAction

continueAction :: IsMode mode => Action emode mode
continueAction = get >>= lift . Brick.continue . AppState

haltAction :: IsMode mode => Action 'Normal mode
haltAction = get >>= lift . Brick.halt . AppState

tickAction :: IsMode mode => Action emode mode
tickAction = fmap (clearPrevKeys . updateTickCounter) <$> continueAction

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

