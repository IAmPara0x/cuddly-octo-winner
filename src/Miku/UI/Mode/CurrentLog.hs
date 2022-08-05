{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
  ( BrickEvent(VtyEvent)
  , EventM
  , Next
  )

import Control.Lens (makeLenses, (<>~), (.~))
import Data.Map             qualified as Map

import Graphics.Vty (Key(KChar, KEsc))
import Graphics.Vty qualified as Vty

import Miku.Templates.Log (Log)

import Miku.UI.State
  ( Action
  , AppState(AppState)
  , execAction
  , IsMode(..)
  , KeyMap
  , Keys
  , Mode(CurrentLogMode)
  , Name
  , Tick
  , SMode(SCurrentLogMode)
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


instance IsMode 'CurrentLogMode where
  type ModeState 'CurrentLogMode = CurrentLogState

  defState _  = return $ CurrentLogState
                          { _clsConfigL =
                              CurrentLogConfig "/home/iamparadox/.miku/" "/home/iamparadox/.miku/logs"
                          , _clsKeyMapL =
                              currentLogStateActions
                          , _clsLogL = Nothing
                          , _clsPrevKeysL = []
                          }

  drawState _ = const [Core.center $ Core.txt "Current Log State"]
  handleEventState = handleCurrentLogStateEvent

  keyMapL   = const clsKeyMapL
  prevKeysL = const clsPrevKeysL


handleCurrentLogStateEvent ::
  SMode 'CurrentLogMode ->
  CurrentLogState ->
  BrickEvent Name Tick ->
  EventM Name (Next AppState)
handleCurrentLogStateEvent mode wstate (eventKey -> Just KEsc)
  = execAction mode (wstate & prevKeysL mode .~ [])
handleCurrentLogStateEvent mode wstate (eventKey -> Just (KChar c))
  = execAction mode (wstate & prevKeysL mode <>~ [c])
handleCurrentLogStateEvent mode wstate  _
  = Brick.continue $ AppState mode wstate

currentLogStateActions :: KeyMap CurrentLogState
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               ]
  where
    
    exitApp :: Action CurrentLogState
    exitApp  = Brick.halt . AppState SCurrentLogMode

eventKey :: BrickEvent Name Tick -> Maybe Vty.Key
eventKey (VtyEvent (Vty.EvKey key _)) = Just key
eventKey _                            = Nothing

toCurrentLogMode :: IsMode a => SMode a -> Action (ModeState a)
toCurrentLogMode _ _ = do 
  def <- liftIO (defState SCurrentLogMode)
  Brick.continue (AppState SCurrentLogMode def)
