{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
  ( BrickEvent(VtyEvent)
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
import Graphics.Vty qualified as Vty

import Miku.UI.Draw.StatusLine (drawStatusLine)
import Miku.UI.State
  ( Action
  , AppState(AppState)
  , execAction
  , IsMode(..)
  , KeyMap
  , Keys
  , Mode(WelcomeMode)
  , Name
  , Tick
  , SMode(SWelcomeMode)
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

instance IsMode 'WelcomeMode where
  type ModeState 'WelcomeMode = WelcomeState
  defState _       = return $ WelcomeState
                               { _wsMsgL = "Moshi Moshi!"
                               , _wsConfigL = WelcomeConfig "/home/iamparadox/.miku/"
                               , _wsKeyMapL = welcomeStateActions
                               , _wsPrevKeysL = []
                               , _wsPrevStateL = Nothing
                               }
  drawState        = const drawWelcomeState
  handleEventState = const handleWelcomeStateEvent
  keyMapL          = const wsKeyMapL
  prevKeysL        = const wsPrevKeysL

handleWelcomeStateEvent :: WelcomeState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleWelcomeStateEvent wstate (eventKey -> Just KEsc)
  = execAction SWelcomeMode (wstate & wsPrevKeysL .~ [])
handleWelcomeStateEvent wstate (eventKey -> Just (KChar c))
  = execAction SWelcomeMode (wstate & wsPrevKeysL <>~ [c])
handleWelcomeStateEvent wstate  _
  = Brick.continue $ AppState SWelcomeMode wstate

eventKey :: BrickEvent Name Tick -> Maybe Vty.Key
eventKey (VtyEvent (Vty.EvKey key _)) = Just key
eventKey _                            = Nothing

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
    changeMsg = Brick.continue . AppState SWelcomeMode . (wsMsgL .~ "welcome!")

    changeMsgAgain :: Action WelcomeState
    changeMsgAgain = Brick.continue . AppState SWelcomeMode . (wsMsgL .~ "welcome again!")

    exitApp :: Action WelcomeState
    exitApp  = Brick.halt . AppState SWelcomeMode
