{-# LANGUAGE ViewPatterns    #-}
module Miku.Mode.Welcome
  ( Welcome
  , toWelcomeMode
  ) where

import Brick.Main           qualified as Brick
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (makeLenses, (.~), (^.))
import Data.Map             qualified as Map

import Miku.Draw.StatusLine (StatusLineInfo (..))
import Miku.Editing         (EditingMode (..))
import Miku.Events          qualified as Events
import Miku.Mode
  ( Action
  , AppState (AppState)
  , DrawMode
  , IsMode (..)
  , KeyMap (..)
  , gsChangeModeL
  , gsModeStateL
  )

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

instance IsMode Welcome where
  type ModeState Welcome = WelcomeState

  defState         = return (WelcomeState { _msgL = "Moshi Moshi!" }, welcomeStateActions)
  drawState        = drawWelcomeState
  handleEventState = Events.handleAnyStateEvent

drawWelcomeState :: DrawMode emode Welcome
drawWelcomeState = do
  gstate <- ask

  let wstate = gstate ^. gsModeStateL
  return (Core.center $ Core.txt (wstate ^. msgL))

welcomeStateActions :: KeyMap Welcome
welcomeStateActions = KeyMap
  { _normalModeMapL = Map.fromList
                        [("c", changeMsg), ("q", Events.halt), ("<spc>w", changeMsgAgain)]
  , _insertModeMapL = mempty
  }
 where

  changeMsg :: Action 'Normal Welcome
  changeMsg = Events.modifyAndContinue (gsModeStateL . msgL .~ "welcome!")

  changeMsgAgain :: Action 'Normal Welcome
  changeMsgAgain = Events.modifyAndContinue (gsModeStateL . msgL .~ "welcome again!")

toWelcomeMode :: forall a . IsMode a => Action 'Normal a
toWelcomeMode = do
  gstate <- get
  wstate <- liftIO $ defState @a
  lift $ Brick.continue (AppState $ gstate & gsChangeModeL .~ wstate)
