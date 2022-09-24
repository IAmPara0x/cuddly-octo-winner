module Miku.Actions
  ( actionWithKeys
  , continue
  , halt
  , handleAnyStateEvent
  , handleInsertStateEvent
  , handleNormalStateEvent
  , modifyAndContinue
  , toInsertMode
  , toNormalMode
  ) where

import Brick.Main    qualified as Brick
import Brick.Types   (BrickEvent (AppEvent, VtyEvent))

import Control.Lens  ((%~), (.~), (<>~), (^.))
import Data.Default  (def)

import Data.Map      qualified as Map

import Graphics.Vty  (Key (KBackTab, KChar, KEsc))
import Graphics.Vty  qualified as Vty

import Miku.Editing  (EditingMode (Insert, Normal), SEditingMode (SInsert, SNormal))
import Miku.Mode     (Action, AppState (..), GlobalState (..), IsMode, Keys)
import Miku.Mode     qualified as Mode
import Miku.Resource (Res, Tick (Tick))

import Relude

handleAnyStateEvent :: IsMode a => BrickEvent Res Tick -> Action emode a
handleAnyStateEvent event = do
  gstate <- get
  case gstate ^. Mode.gsEditingModeL of
    SNormal -> handleNormalStateEvent event
    SInsert -> handleInsertStateEvent event

handleNormalStateEvent :: forall a . IsMode a => BrickEvent Res Tick -> Action 'Normal a
handleNormalStateEvent (AppEvent Tick              ) = tickAction
handleNormalStateEvent (VtyEvent (Vty.EvKey key [])) = case key of
  KEsc         -> modify ((Mode.gsPrevKeysL .~ []) . (Mode.gsKeysTickCounterL .~ 0)) >> continue
  (KChar '\t') -> actionWithKeys "<tab>"
  (KChar ' ' ) -> actionWithKeys "<spc>"
  (KChar 'i' ) -> toInsertMode
  (KChar c   ) -> actionWithKeys [c]
  KBackTab     -> actionWithKeys "<shift>+<tab>"
  _            -> continue
handleNormalStateEvent _ = continue

handleInsertStateEvent :: forall a . IsMode a => BrickEvent Res Tick -> Action 'Insert a
handleInsertStateEvent (AppEvent Tick)                     = tickAction
handleInsertStateEvent (VtyEvent (Vty.EvKey KEsc []))      = toNormalMode
handleInsertStateEvent (VtyEvent (Vty.EvKey (KChar c) [])) = actionWithKeys [c]
handleInsertStateEvent _                                   = continue

continue :: IsMode mode => Action emode mode
continue = get >>= Mode.liftEvent . Brick.continue . AppState

modifyAndContinue :: IsMode m => (GlobalState e m -> GlobalState e m) -> Action e m
modifyAndContinue f = modify f >> continue

halt :: IsMode mode => Action 'Normal mode
halt = get >>= Mode.liftEvent . Brick.halt . AppState

tickAction :: IsMode mode => Action emode mode
tickAction = fmap (clearPrevKeys . updateTickCounter) <$> continue
 where
  updateTickCounter :: AppState -> AppState
  updateTickCounter (AppState gstate) =
    AppState
      $  gstate
      &  Mode.gsTickCounterL
      %~ (\tickCounts -> mod (tickCounts + 1) (Mode._gcMaxTickCounterL def))
      &  Mode.gsKeysTickCounterL
      %~ (\ktickCounts -> mod (ktickCounts + 1) (Mode._gcMaxTickCounterL def))

  clearPrevKeys :: AppState -> AppState
  clearPrevKeys (AppState gstate) = AppState $ gstate & Mode.gsPrevKeysL %~ bool
    id
    (const [])
    (rem (_gsKeysTickCounterL gstate) (Mode._gcClearKeysTimeL def) == 0)

actionWithKeys :: forall a emode . IsMode a => Keys -> Action emode a
actionWithKeys keys = do
  modify ((Mode.gsPrevKeysL <>~ keys) . (Mode.gsKeysTickCounterL .~ 0))
  gstate <- get

  case Map.lookup (gstate ^. Mode.gsPrevKeysL) (Mode.getKeyMap gstate) of
    Just action -> fmap (Mode.modifyAppState $ Mode.gsPrevKeysL .~ []) <$> action
    Nothing     -> continue

toInsertMode :: IsMode a => Action 'Normal a
toInsertMode = do
  gstate <- get
  Mode.liftEvent $ Brick.continue (AppState $ Mode.changeEditingMode SInsert gstate)

toNormalMode :: IsMode a => Action 'Insert a
toNormalMode = do
  gstate <- get
  Mode.liftEvent $ Brick.continue (AppState $ Mode.changeEditingMode SNormal gstate)
