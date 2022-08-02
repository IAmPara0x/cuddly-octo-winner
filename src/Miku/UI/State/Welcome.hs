{-# LANGUAGE TemplateHaskell #-}

module Miku.UI.State.Welcome
  ( drawWelcomeState
  , WelcomeConfig(WelcomeConfig)
  , wcConfigPathL
  , WelcomeState(WelcomeState)
  , wsMsgL
  )
  where

import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core (txt)

import Control.Lens ((^.), makeLenses)

import Relude

newtype WelcomeConfig =
  WelcomeConfig { _wcConfigPathL :: FilePath }
  deriving stock (Show)

newtype WelcomeState =
  WelcomeState { _wsMsgL :: Text
               } deriving stock (Show)

makeLenses ''WelcomeConfig
makeLenses ''WelcomeState

drawWelcomeState :: WelcomeConfig -> WelcomeState -> [Widget n]
drawWelcomeState _wconfig wstate = [txt (wstate ^. wsMsgL)]
