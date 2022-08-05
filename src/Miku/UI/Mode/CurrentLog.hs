{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Miku.UI.Mode.CurrentLog
  ( clcConfigPathL
  , clcLogsDirL
  , clsLogL
  , clsConfigL
  , CurrentLogConfig(..)
  , CurrentLogState(..)
  , toCurrentLogMode
  )
  where

import Brick.Main           qualified as Brick
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core
import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  )

import Control.Lens (makeLenses, (<>~), (.~))
import Data.Map             qualified as Map

import Graphics.Vty (Key(KChar, KEsc))

import Miku.Templates.Log (Log)

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


data CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL :: FilePath
                   , _clcLogsDirL    :: FilePath
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL   :: CurrentLogConfig
                  , _clsKeyMapL   :: KeyMap CurrentLogState
                  , _clsLogL      :: Maybe Log
                  , _clsPrevKeysL :: Keys
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState


instance IsMode CurrentLogState where

  defState = return $ CurrentLogState
                          { _clsConfigL =
                              CurrentLogConfig "/home/iamparadox/.miku/" "/home/iamparadox/.miku/logs"
                          , _clsKeyMapL =
                              currentLogStateActions
                          , _clsLogL = Nothing
                          , _clsPrevKeysL = []
                          }

  drawState = const [Core.center $ Core.txt "Current Log State"]
  handleEventState = handleCurrentLogStateEvent

  keyMapL   = clsKeyMapL
  prevKeysL = clsPrevKeysL


handleCurrentLogStateEvent :: CurrentLogState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleCurrentLogStateEvent wstate (eventKey -> Just KEsc)      = execAction (wstate & prevKeysL .~ [])
handleCurrentLogStateEvent wstate (eventKey -> Just (KChar c)) = execAction (wstate & prevKeysL <>~ [c])
handleCurrentLogStateEvent wstate  _                           = Brick.continue $ AppState wstate

currentLogStateActions :: KeyMap CurrentLogState
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               ]
  where

    exitApp :: Action CurrentLogState
    exitApp  = Brick.halt . AppState

toCurrentLogMode :: forall a. IsMode a => Action a
toCurrentLogMode _ = liftIO (defState @CurrentLogState) >>= Brick.continue . AppState
