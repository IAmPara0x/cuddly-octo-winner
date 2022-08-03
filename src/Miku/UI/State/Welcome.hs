{-# LANGUAGE TemplateHaskell #-}

module Miku.UI.State.Welcome
  ( WelcomeConfig(..)
  , wcConfigPathL
  , WelcomeState(..)
  , wsConfigL
  , wsMsgL
  , wsPrevKeysL
  , wsActionsL
  , welcomeStateActions
  )
  where


import Control.Lens (makeLenses, (.~))
import Data.Default (Default(def))
import Data.Map qualified as Map

import Relude

type KeyMap = [Char]

newtype WelcomeConfig =
  WelcomeConfig { _wcConfigPathL :: FilePath
                } deriving stock (Show)

data WelcomeState =
  WelcomeState { _wsMsgL      :: Text
               , _wsConfigL   :: WelcomeConfig
               , _wsActionsL  :: Map KeyMap (WelcomeState -> WelcomeState)
               , _wsPrevKeysL :: KeyMap
               }


makeLenses ''WelcomeConfig
makeLenses ''WelcomeState

clearPrevKeys :: WelcomeState -> WelcomeState
clearPrevKeys = wsPrevKeysL .~ []

welcomeStateActions :: Map KeyMap (WelcomeState -> WelcomeState)
welcomeStateActions =
  Map.fromList [ (" wj", clearPrevKeys . (wsMsgL .~ "Welcome Again"))
               , ("c", clearPrevKeys . (wsMsgL .~ "welcome!"))
               ]

instance Default WelcomeState where
  def = WelcomeState { _wsMsgL = "Moshi Moshi!"
                     , _wsConfigL = WelcomeConfig "/home/iamparadox/.miku/"
                     , _wsActionsL = welcomeStateActions
                     , _wsPrevKeysL = []
                     }
