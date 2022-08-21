{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE TypeFamilies    #-}

module Miku.UI.Mode.Welcome
  ( 
    WelcomeConfig(..)
  , wcConfigPathL
  , WelcomeState(..)
  , wsConfigL
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
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , Keys
  , DrawMode
  )

import Relude


data Welcome

-- | Welcome State

newtype WelcomeConfig =
  WelcomeConfig { _wcConfigPathL :: FilePath
                } deriving stock (Show)

data WelcomeState =
    WelcomeState { _wsConfigL    :: WelcomeConfig
                 , _wsKeyMapL    :: KeyMap Welcome
                 , _wsMsgL       :: Text
                 , _wsPrevKeysL  :: Keys
                 }
makeLenses ''WelcomeConfig
makeLenses ''WelcomeState

instance IsMode Welcome where
  type ModeState Welcome = WelcomeState

  defState = return $ WelcomeState
                               { _wsMsgL = "Moshi Moshi!"
                               , _wsConfigL = WelcomeConfig "/home/iamparadox/.miku/"
                               , _wsKeyMapL = welcomeStateActions
                               , _wsPrevKeysL = []
                               }
  drawState        = drawWelcomeState
  handleEventState = handleAnyStateEvent @Welcome
  keyMapL          = wsKeyMapL
  prevKeysL        = wsPrevKeysL

drawWelcomeState :: DrawMode Welcome
drawWelcomeState  = do
  wstate <- ask

  return [ Core.vBox
             [ Core.vLimitPercent 94 $ Core.center $ Core.txt (wstate ^. wsMsgL)
             , drawStatusLine (Text.pack $ wstate ^. prevKeysL @Welcome) ""
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
    changeMsg = ask >>= lift . Brick.continue . (wsMsgL .~ "welcome!") 

    changeMsgAgain :: Action Welcome
    changeMsgAgain = ask >>= lift . Brick.continue . (wsMsgL .~ "welcome again!")

    exitApp :: Action Welcome
    exitApp  = ask >>= lift . Brick.halt
