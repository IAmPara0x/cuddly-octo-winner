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

import Brick.Types
  ( Widget
  )

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
  )

import Relude


-- | Welcome State

newtype WelcomeConfig =
  WelcomeConfig { _wcConfigPathL :: FilePath
                } deriving stock (Show)

data WelcomeState =
    WelcomeState { _wsConfigL    :: WelcomeConfig
                 , _wsKeyMapL    :: KeyMap WelcomeState
                 , _wsMsgL       :: Text
                 , _wsPrevKeysL  :: Keys
                 }

makeLenses ''WelcomeConfig
makeLenses ''WelcomeState

instance IsMode WelcomeState where
  defState = return $ WelcomeState
                               { _wsMsgL = "Moshi Moshi!"
                               , _wsConfigL = WelcomeConfig "/home/iamparadox/.miku/"
                               , _wsKeyMapL = welcomeStateActions
                               , _wsPrevKeysL = []
                               }
  drawState        = drawWelcomeState
  handleEventState = handleAnyStateEvent
  keyMapL          = wsKeyMapL
  prevKeysL        = wsPrevKeysL

drawWelcomeState :: WelcomeState -> [Widget n]
drawWelcomeState wstate =
  [ Core.vBox
     [ Core.vLimitPercent 94 $ Core.center $ Core.txt (wstate ^. wsMsgL)
     , drawStatusLine (Text.pack $ wstate ^. prevKeysL) ""
     ]
  ]

welcomeStateActions :: KeyMap WelcomeState
welcomeStateActions =
  Map.fromList [ ("c", changeMsg)
               , ("q", exitApp)
               , ("<spc>w", changeMsgAgain)
               ]
  where

    changeMsg :: Action WelcomeState
    changeMsg = ask >>= lift . Brick.continue . (wsMsgL .~ "welcome!") 

    changeMsgAgain :: Action WelcomeState
    changeMsgAgain = ask >>= lift . Brick.continue . (wsMsgL .~ "welcome again!")

    exitApp :: Action WelcomeState
    exitApp  = ask >>= lift . Brick.halt

-- toWelcomeMode :: Action WelcomeState
-- toWelcomeMode = liftIO (defState @WelcomeState)
