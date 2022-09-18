{-# LANGUAGE ViewPatterns    #-}
module Miku.Mode.Welcome
  ( Welcome
  , initWelcomeMode
  , toWelcomeMode
  ) where

import Brick.Main           qualified as Brick
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (makeLenses, (.~), (^.))
import Data.Default         (Default (def))
import Data.Map             qualified as Map

import Miku.Draw.StatusLine (StatusLineInfo (..))
import Miku.Editing         (EditingMode (..))
import Miku.Events          qualified as Events
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


data Welcome
  = Welcome
  deriving stock (Show)

instance StatusLineInfo Welcome where
  statusLineInfo a = [show a]

-- | Welcome State

newtype WelcomeState
  = WelcomeState { _msgL :: Text }
makeLenses ''WelcomeState

instance Default WelcomeState where
  def = WelcomeState "Moshi Moshi"

instance IsMode Welcome where
  type ModeState Welcome = WelcomeState
  type ModeConf Welcome = ()

  drawstate        = drawWelcomeState
  handleEventState = Events.handleAnyStateEvent

toWelcomeMode :: forall a . Action 'Normal a
toWelcomeMode = do
  gstate <- get
  lift $ Brick.continue (AppState $ gstate & Mode.gsModeL .~ (def, (), welcomeStateActions))

initWelcomeMode :: IO (GlobalState 'Normal Welcome)
initWelcomeMode = do
  return $ Mode.initGlobalState def () welcomeStateActions

drawWelcomeState :: DrawMode emode Welcome
drawWelcomeState = do
  gstate <- ask

  let wstate = gstate ^. Mode.gsModeStateL
  return (Core.center $ Core.txt (wstate ^. msgL))

welcomeStateActions :: KeyMap Welcome
welcomeStateActions = KeyMap
  { _normalModeMapL = Map.fromList
                        [("c", changeMsg), ("q", Events.halt), ("<spc>w", changeMsgAgain)]
  , _insertModeMapL = mempty
  }
 where

  changeMsg :: Action 'Normal Welcome
  changeMsg = Events.modifyAndContinue (Mode.gsModeStateL . msgL .~ "welcome!")

  changeMsgAgain :: Action 'Normal Welcome
  changeMsgAgain = Events.modifyAndContinue (Mode.gsModeStateL . msgL .~ "welcome again!")
