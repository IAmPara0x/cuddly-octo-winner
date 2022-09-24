module Miku.Mode.Default
  ( initDefaultMode
  , toDefaultMode
  ) where

import Brick.Main           qualified as Brick
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (makeLenses, (.~), (^.))
import Data.Default         (Default (def))
import Data.Map             qualified as Map

import Miku.Actions         qualified as Actions
import Miku.Editing         (EditingMode (..))
import Miku.Mode
  ( Action
  , AppState (AppState)
  , DrawMode
  , GlobalState
  , IsMode (..)
  , KeyMap (..)
  )
import Miku.Mode            qualified as Mode

import Relude


data DefaultMode

newtype DefaultState
  = DefaultState { _msgL :: Text }
makeLenses ''DefaultState

instance Default DefaultState where
  def = DefaultState "Moshi Moshi"

instance IsMode DefaultMode where
  type ModeState DefaultMode = DefaultState
  type ModeConf DefaultMode = ()

  drawstate        = drawDefaultState
  handleEventState = Actions.handleAnyStateEvent

toDefaultMode :: forall a . Action 'Normal a
toDefaultMode = do
  gstate <- get
  Mode.liftEvent
    $ Brick.continue (AppState $ gstate & Mode.gsModeL .~ (def, (), welcomeStateActions))

initDefaultMode :: IO (GlobalState 'Normal DefaultMode)
initDefaultMode = do
  return $ Mode.initGlobalState def () welcomeStateActions

drawDefaultState :: DrawMode emode DefaultMode
drawDefaultState = do
  gstate <- ask
  let wstate = gstate ^. Mode.gsModeStateL
  return (Core.center $ Core.txt (wstate ^. msgL))

welcomeStateActions :: KeyMap DefaultMode
welcomeStateActions = KeyMap
  { _normalModeMapL = Map.fromList
                        [("c", changeMsg), ("q", Actions.halt), ("<spc>w", changeMsgAgain)]
  , _insertModeMapL = mempty
  }
 where

  changeMsg :: Action 'Normal DefaultMode
  changeMsg = Actions.modifyAndContinue (Mode.gsModeStateL . msgL .~ "welcome!")

  changeMsgAgain :: Action 'Normal DefaultMode
  changeMsgAgain = Actions.modifyAndContinue (Mode.gsModeStateL . msgL .~ "welcome again!")
