{-# LANGUAGE ViewPatterns    #-}
module Miku.Mode.Welcome
  ( WelcomeState(..)
  , Welcome
  , wsMsgL
  , toWelcomeMode
  )
  where

import Brick.Main qualified as Brick
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens (makeLenses, (^.), (.~))
import Data.Map qualified as Map

import Miku.Draw.StatusLine (StatusLine(..))
import Miku.Draw(draw, W, Draw(..))
import Miku.Mode
  ( Action
  , AppState(AppState)
  , continueAction
  , haltAction
  , gsModeStateL
  , gsChangeModeL
  , gsEditingModeL
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , DrawMode
  )

import Relude


data Welcome

-- | Welcome State

newtype WelcomeState =
    WelcomeState { _wsMsgL       :: Text
                 }
makeLenses ''WelcomeState

instance IsMode Welcome where
  type ModeState Welcome = WelcomeState

  defState         = return (WelcomeState { _wsMsgL = "Moshi Moshi!" }, welcomeStateActions)
  drawState        = drawWelcomeState
  handleEventState = handleAnyStateEvent

drawWelcomeState :: DrawMode Welcome
drawWelcomeState  = do
  gstate <- ask

  let wstate = gstate ^. gsModeStateL

  return [ Core.vBox
             [ Core.center $ Core.txt (wstate ^. wsMsgL)
             , draw $ runReader statusLine gstate
             ]
         ]

statusLine :: W Welcome StatusLine
statusLine = do
  gstate <- ask

  let widget = StatusLine { _slEditingModeL = gstate ^. gsEditingModeL
                          , _slModeNameL = "Welcome"
                          , _slOtherInfoL = []
                          }

  return Draw { _focusedL = False , _drawableL = widget, _borderTypeL = Border.unicode }

welcomeStateActions :: KeyMap Welcome
welcomeStateActions =
  Map.fromList [ ("c", changeMsg)
               , ("q", exitApp)
               , ("<spc>w", changeMsgAgain)
               ]
  where

    changeMsg :: Action Welcome
    changeMsg = modify (gsModeStateL . wsMsgL .~ "welcome!") >> continueAction

    changeMsgAgain :: Action Welcome
    changeMsgAgain = modify (gsModeStateL . wsMsgL .~ "welcome again!") >> continueAction

    exitApp :: Action Welcome
    exitApp  = haltAction

toWelcomeMode :: forall a. IsMode a => Action a
toWelcomeMode = do
    gstate <- get
    wstate <- liftIO $ defState @Welcome
    lift $ Brick.continue (AppState $ gstate & gsChangeModeL .~ wstate)
