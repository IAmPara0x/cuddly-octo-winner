{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Miku.UI.State
  ( AppState(WState, CLState)
  , CurrentLogConfig(..)
  , clcConfigPathL
  , clcLogsDirL
  , CurrentLogState(..)
  , clsActionsL
  , clsConfigL
  , clsLogL
  , clsPrevKeysL
  , Name
  , Tick
  , WelcomeConfig(..)
  , wcConfigPathL
  , WelcomeState(..)
  , wsConfigL
  , wsMsgL
  , wsPrevKeysL
  , wsActionsL
  , welcomeStateActions
  )
  where

import Brick.Main qualified as Brick
import Brick.Types (EventM, Next)

import Control.Lens (makeLenses, (.~))
import Data.Default (Default(def))
import Data.Map qualified as Map

import Miku.Templates.Log (Log)

import Relude

data AppState where
  WState :: WelcomeState -> AppState
  CLState :: CurrentLogState -> AppState

type Name = ()
type Tick = ()

type Action a = (a -> EventM Name (Next AppState))

-- | Welcome State

type KeyMap = [Char]

newtype WelcomeConfig =
  WelcomeConfig { _wcConfigPathL :: FilePath
                } deriving stock (Show)

data WelcomeState =
  WelcomeState { _wsActionsL  :: Map KeyMap (Action WelcomeState)
               , _wsConfigL   :: WelcomeConfig
               , _wsMsgL      :: Text
               , _wsPrevKeysL :: KeyMap
               }

-- | Current Log State
data CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL :: FilePath
                   , _clcLogsDirL    :: FilePath
                   } deriving stock (Show)

data CurrentLogState =
  CurrentLogState { _clsActionsL  :: Map KeyMap (Action CurrentLogState)
                  , _clsConfigL   :: CurrentLogConfig
                  , _clsLogL      :: Maybe Log
                  , _clsPrevKeysL :: KeyMap
                  }


makeLenses ''WelcomeConfig
makeLenses ''WelcomeState

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState


-- | Welcome State

clearPrevKeys :: WelcomeState -> WelcomeState
clearPrevKeys = wsPrevKeysL .~ []

welcomeStateActions :: Map KeyMap (Action WelcomeState)
welcomeStateActions =
  Map.fromList [ ("c", changeMsg)
               , ("log", toCurrentLogState)
               ]
  where

    changeMsg :: Action WelcomeState
    changeMsg = Brick.continue . WState . clearPrevKeys . (wsMsgL .~ "welcome!")

    toCurrentLogState :: Action WelcomeState
    toCurrentLogState = Brick.continue . const (CLState def)

instance Default WelcomeState where
  def = WelcomeState { _wsMsgL = "Moshi Moshi!"
                     , _wsConfigL = WelcomeConfig "/home/iamparadox/.miku/"
                     , _wsActionsL = welcomeStateActions
                     , _wsPrevKeysL = []
                     }

-- | Current Log State

currentLogStateActions :: Map KeyMap (Action CurrentLogState)
currentLogStateActions =
  Map.fromList [ (" ws", toWelcomeState)
               ]
  where

    toWelcomeState :: Action CurrentLogState
    toWelcomeState = Brick.continue . const (WState $ def & (wsMsgL .~ "Welcome Again!"))

instance Default CurrentLogState where
  def = CurrentLogState { _clsLogL      = Nothing
                        , _clsActionsL  = currentLogStateActions
                        , _clsPrevKeysL = [] 
                        , _clsConfigL   =
                            CurrentLogConfig { _clcConfigPathL = "/home/iamparadox/.miku/"
                                             , _clcLogsDirL = "/home/iamparadox/.miku/logs"
                                             }
                        }
