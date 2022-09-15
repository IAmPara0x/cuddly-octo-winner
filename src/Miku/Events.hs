module Miku.Events
  ( actionWithKeys
  , continueAction
  , haltAction
  , handleAnyStateEvent
  , handleInsertStateEvent
  , handleNormalStateEvent
  , toInsertMode
  , toNormalMode
  ) where

import Brick.Main   qualified as Brick
import Brick.Types  (BrickEvent (AppEvent, VtyEvent))

import Control.Lens (_1, _2, (%~), (+~), (.~), (<>~), (^.))

import Data.Map     qualified as Map

import Graphics.Vty (Key (KBackTab, KChar, KEsc))
import Graphics.Vty qualified as Vty

import Miku.Editing (EditingMode (Insert, Normal),
                     SEditingMode (SInsert, SNormal))
import Miku.Mode    (Action, AppState (..), IsMode, Keys, Name, Tick (..),
                     clearKeysL, getKeyMap, gsEditingModeL, gsKeysTickCounterL,
                     gsKeysTickL, gsPrevKeysL, gsTickCounterL, gsTickL)

import Relude

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

continueAction :: IsMode mode => Action emode mode
continueAction = get >>= lift . Brick.continue . AppState

haltAction :: IsMode mode => Action 'Normal mode
haltAction = get >>= lift . Brick.halt . AppState

tickAction :: IsMode mode => Action emode mode
tickAction = fmap (clearPrevKeys . updateTickCounter) <$> continueAction
  where
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

actionWithKeys :: forall a emode. IsMode a => Keys -> Action emode a
actionWithKeys keys = do
  modify ((gsPrevKeysL <>~ keys) . (gsKeysTickCounterL .~ 0))
  gstate <- get

  case Map.lookup (gstate ^. gsPrevKeysL) (getKeyMap gstate) of
    Just action -> fmap (clearKeysL .~ []) <$> action
    Nothing     -> continueAction

toInsertMode :: IsMode a => Action 'Normal a
toInsertMode = do
  gstate <- get
  lift $ Brick.continue (AppState $ gstate & gsEditingModeL .~ SInsert)

toNormalMode :: IsMode a => Action 'Insert a
toNormalMode = do
  gstate <- get
  lift $ Brick.continue (AppState $ gstate & gsEditingModeL .~ SNormal)
