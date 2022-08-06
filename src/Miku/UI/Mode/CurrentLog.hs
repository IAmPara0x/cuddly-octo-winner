{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Miku.UI.Mode.CurrentLog
  ( clcConfigPathL
  , clsLogL
  , clsConfigL
  , CurrentLogConfig(..)
  , CurrentLogState(..)
  , toCurrentLogMode
  )
  where

import Brick.Main           qualified as Brick
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Brick.Types
  ( BrickEvent
  , EventM
  , Next
  , Widget
  )

import Control.Lens (makeLenses, (<>~), (^.), (.~))
import Data.Map             qualified as Map
import Data.Text            qualified as Text

import Graphics.Vty (Key(KChar, KEsc))

import Miku.Templates.Log
  ( Heading
  , logHeadingL
  , logGoalsL
  , Log
  , readCurrentLog
  , showHeading
  )

import Miku.UI.Draw.CurrentTask (drawCurrentTask)
import Miku.UI.Draw.Goals       (drawGoals)
import Miku.UI.Draw.StatusLine  (drawStatusLine)
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

import System.FilePath ((</>))

import Relude


newtype CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL :: FilePath
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL   :: CurrentLogConfig
                  , _clsKeyMapL   :: KeyMap CurrentLogState
                  , _clsLogL      :: Log
                  , _clsPrevKeysL :: Keys
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState


instance IsMode CurrentLogState where

  defState         = defCurrentLogState
  drawState        = drawCurrentLogState
  handleEventState = handleCurrentLogStateEvent

  keyMapL          = clsKeyMapL
  prevKeysL        = clsPrevKeysL


defCurrentLogState :: IO CurrentLogState
defCurrentLogState = 
  do

    let
        configPath :: FilePath
        configPath = "/home/iamparadox/.miku/"

        logsDir :: FilePath
        logsDir = configPath </> "logs"

    elog <- runExceptT $ readCurrentLog logsDir

    case elog of
      Left err  -> error err
      Right log -> return $ CurrentLogState
                          { _clsConfigL   = CurrentLogConfig configPath
                          , _clsKeyMapL   = currentLogStateActions
                          , _clsLogL      = log
                          , _clsPrevKeysL = []
                          }

handleCurrentLogStateEvent :: CurrentLogState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleCurrentLogStateEvent wstate (eventKey -> Just KEsc)         = execAction (wstate & prevKeysL .~ [])
handleCurrentLogStateEvent wstate (eventKey -> Just (KChar '\t')) = execAction (wstate & prevKeysL <>~ "<tab>")
handleCurrentLogStateEvent wstate (eventKey -> Just (KChar c))    = execAction (wstate & prevKeysL <>~ [c])
handleCurrentLogStateEvent wstate  _                              = Brick.continue $ AppState wstate

currentLogStateActions :: KeyMap CurrentLogState
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               ]
  where

    exitApp :: Action CurrentLogState
    exitApp  = Brick.halt . AppState

toCurrentLogMode :: forall a. IsMode a => Action a
toCurrentLogMode _ = liftIO (defState @CurrentLogState) >>= Brick.continue . AppState



drawCurrentLogState :: CurrentLogState -> [Widget n]
drawCurrentLogState s =
  [ Core.vBox
      [ Core.vLimitPercent 94 $
          Core.vBox [ drawHeading (s ^. clsLogL . logHeadingL)
                    , drawCurrentTask True (Left "No Task")
                    , drawGoals (s ^. clsLogL . logGoalsL)
                    ]
      , drawStatusLine (Text.pack $ s ^. prevKeysL) ""
      ]
  ]

drawHeading :: Heading -> Widget n
drawHeading h =
  Core.vBox [ Core.padAll 1 $ Core.hCenter $ Core.txt $ showHeading h
            ]
