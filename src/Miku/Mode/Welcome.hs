{-# LANGUAGE ViewPatterns    #-}
module Miku.Mode.Welcome
  ( Welcome
  , WelcomeState (..)
  , toWelcomeMode
  , wsMsgL
  ) where

import Brick.Main           qualified as Brick
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (makeLenses, (.~), (^.))
import Data.Map             qualified as Map

import Miku.Draw.StatusLine (StatusLineInfo (..))
import Miku.Editing         (EditingMode (..))
import Miku.Events          (continueAction, haltAction, handleAnyStateEvent)
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
  = WelcomeState { _wsMsgL :: Text }
makeLenses ''WelcomeState

instance IsMode Welcome where
  type ModeState Welcome = WelcomeState

  defState         = return (WelcomeState { _wsMsgL = "Moshi Moshi!" }, welcomeStateActions)
  drawState        = drawWelcomeState
  handleEventState = handleAnyStateEvent

drawWelcomeState :: DrawMode emode Welcome
drawWelcomeState = do
  gstate <- ask

  let wstate = gstate ^. gsModeStateL
  return (Core.center $ Core.txt (wstate ^. wsMsgL))

welcomeStateActions :: KeyMap Welcome
welcomeStateActions = KeyMap
  { _normalModeMapL = Map.fromList [("c", changeMsg), ("q", exitApp), ("<spc>w", changeMsgAgain)]
  , _insertModeMapL = mempty
  }
 where

  changeMsg :: Action 'Normal Welcome
  changeMsg = modify (gsModeStateL . wsMsgL .~ "welcome!") >> continueAction

  changeMsgAgain :: Action 'Normal Welcome
  changeMsgAgain = modify (gsModeStateL . wsMsgL .~ "welcome again!") >> continueAction

  exitApp :: Action 'Normal Welcome
  exitApp = haltAction

toWelcomeMode :: forall a . IsMode a => Action 'Normal a
toWelcomeMode = do
  gstate <- get
  wstate <- liftIO $ defState @a
  lift $ Brick.continue (AppState $ gstate & gsChangeModeL .~ wstate)
