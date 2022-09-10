{-# LANGUAGE ViewPatterns    #-}
module Miku.Mode.Welcome
  ( Welcome
  , WelcomeState (..)
  , toWelcomeMode
  , wsMsgL
  ) where

import Brick.Main                 qualified as Brick
import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Center       qualified as Core
import Brick.Widgets.Core         qualified as Core

import Control.Lens               (makeLenses, (.~), (^.))
import Data.Map                   qualified as Map

import Miku.Draw                  (Draw (..), W, draw)
import Miku.Draw.StatusLine       (StatusLine (..))
import Miku.Editing               (EditingMode (..))
import Miku.Mode                  (Action, AppState (AppState), DrawMode,
                                   IsMode (..), KeyMap (..), continueAction,
                                   gsChangeModeL, gsEditingModeL, gsModeStateL,
                                   haltAction, handleAnyStateEvent)

import Relude


data Welcome

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
drawWelcomeState  = do
  gstate <- ask

  let wstate = gstate ^. gsModeStateL

  return [ Core.vBox
             [ Core.center $ Core.txt (wstate ^. wsMsgL)
             , draw $ runReader statusLine gstate
             ]
         ]

statusLine :: W emode Welcome (StatusLine emode)
statusLine = do
  gstate <- ask

  let widget = StatusLine { _slEditingModeL = gstate ^. gsEditingModeL
                          , _slModeNameL = "Welcome"
                          , _slOtherInfoL = []
                          }

  return Draw { _focusedL = False , _drawableL = widget, _borderTypeL = Border.unicode }

welcomeStateActions :: KeyMap Welcome
welcomeStateActions =
  KeyMap { _normalModeMapL =
              Map.fromList [ ("c", changeMsg)
                           , ("q", exitApp)
                           , ("<spc>w", changeMsgAgain)
                           ]
         , _insertModeMapL = mempty
         }
  where

    changeMsg :: Action 'Normal Welcome
    changeMsg = modify (gsModeStateL . wsMsgL .~ "welcome!") >> continueAction

    changeMsgAgain :: Action 'Normal Welcome
    changeMsgAgain = modify (gsModeStateL . wsMsgL .~ "welcome again!") >> continueAction

    exitApp :: Action 'Normal Welcome
    exitApp  = haltAction

toWelcomeMode :: Action 'Normal a
toWelcomeMode = do
    gstate <- get
    wstate <- liftIO $ defState @Welcome
    lift $ Brick.continue (AppState $ gstate & gsChangeModeL .~ wstate)
