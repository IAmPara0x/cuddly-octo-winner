{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE TypeFamilies    #-}

module Miku.UI.Mode.Welcome
  ( WelcomeState(..)
  , wsMsgL
  )
  where

import Brick.Main qualified as Brick
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core


import Control.Lens (makeLenses, (^.), (.~))
import Data.Map qualified as Map
import Data.Text qualified as Text


import Miku.UI.Draw.StatusLine (drawStatusLine)
import Miku.UI.State
  ( Action
  , AppState(AppState)
  , gsModeStateL
  , gsPrevKeysL
  , gsKeyMapL
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

  defState = return $ WelcomeState
                               { _wsMsgL = "Moshi Moshi!"
                               }
  drawState        = drawWelcomeState
  handleEventState = handleAnyStateEvent @Welcome . (gsKeyMapL .~ welcomeStateActions)

drawWelcomeState :: DrawMode Welcome
drawWelcomeState  = do
  gstate <- ask

  let wstate = gstate ^. gsModeStateL

  return [ Core.vBox
             [ Core.vLimitPercent 94 $ Core.center $ Core.txt (wstate ^. wsMsgL)
             , drawStatusLine (Text.pack $ gstate ^. gsPrevKeysL @Welcome) ""
             ]
         ]

welcomeStateActions :: KeyMap Welcome
welcomeStateActions =
  Map.fromList [ ("c", changeMsg)
               , ("q", exitApp)
               , ("<spc>w", changeMsgAgain)
               ]
  where

    changeMsg :: Action Welcome
    changeMsg = ask >>= lift . Brick.continue
                             . AppState Proxy
                             . (gsModeStateL . wsMsgL .~ "welcome!") 

    changeMsgAgain :: Action Welcome
    changeMsgAgain = ask >>= lift . Brick.continue
                                  . AppState Proxy
                                  . (gsModeStateL . wsMsgL .~ "welcome again!")

    exitApp :: Action Welcome
    exitApp  = ask >>= lift . Brick.halt . AppState Proxy

-- toWelcomeMode :: Action a
-- toWelcomeMode = do
--   gstate <- ask
--   lift $ Brick.continue
--        $ AppState Proxy
