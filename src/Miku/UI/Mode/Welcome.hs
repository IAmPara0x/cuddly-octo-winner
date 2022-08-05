{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Miku.UI.Mode.Welcome
  ( WelcomeConfig(..)
  , wcConfigPathL
  , WelcomeState(..)
  , wsConfigL
  , wsMsgL
  , wsPrevStateL
  )
  where

import Brick.Main qualified as Brick

import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  , Widget
  )

import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core


import Control.Lens (makeLenses, (^.), (<>~), (.~))
import Data.Map qualified as Map
import Data.Text qualified as Text

import Graphics.Vty (Key(KChar, KEsc))

import Miku.UI.Draw.StatusLine (drawStatusLine)
import Miku.UI.State
  ( Action
  , AppState(AppState)
  , eventKey
  , execAction
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick
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
                 , _wsPrevStateL :: Maybe AppState
                 }

makeLenses ''WelcomeConfig
makeLenses ''WelcomeState

instance IsMode WelcomeState where
  defState = return $ WelcomeState
                               { _wsMsgL = "Moshi Moshi!"
                               , _wsConfigL = WelcomeConfig "/home/iamparadox/.miku/"
                               , _wsKeyMapL = welcomeStateActions
                               , _wsPrevKeysL = []
                               , _wsPrevStateL = Nothing
                               }
  drawState        = drawWelcomeState
  handleEventState = handleWelcomeStateEvent
  keyMapL          = wsKeyMapL
  prevKeysL        = wsPrevKeysL

handleWelcomeStateEvent :: WelcomeState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleWelcomeStateEvent wstate (eventKey -> Just KEsc)      = execAction (wstate & wsPrevKeysL .~ [])
handleWelcomeStateEvent wstate (eventKey -> Just (KChar c)) = execAction (wstate & wsPrevKeysL <>~ [c])
handleWelcomeStateEvent wstate  _                           = Brick.continue $ AppState wstate

drawWelcomeState :: WelcomeState -> [Widget n]
drawWelcomeState wstate =
  [Core.vBox
    [ Core.vLimitPercent 95 $ Core.center $ Core.txt (wstate ^. wsMsgL)
    , drawStatusLine ("CMD: " <> Text.pack (wstate ^. wsPrevKeysL))
    ]
  ]

welcomeStateActions :: KeyMap WelcomeState
welcomeStateActions =
  Map.fromList [ ("c", changeMsg)
               , ("q", exitApp)
               , (" w", changeMsgAgain)
               ]
  where

    changeMsg :: Action WelcomeState
    changeMsg = Brick.continue . AppState . (wsMsgL .~ "welcome!")

    changeMsgAgain :: Action WelcomeState
    changeMsgAgain = Brick.continue . AppState . (wsMsgL .~ "welcome again!")

    exitApp :: Action WelcomeState
    exitApp  = Brick.halt . AppState
