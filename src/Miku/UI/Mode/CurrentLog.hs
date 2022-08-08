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

import Brick.Main                  qualified as Brick
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Brick.Types
  ( Widget
  , Padding(Pad)
  )

import Control.Lens (makeLenses, (^.))
import Data.Map             qualified as Map
import Data.Text            qualified as Text


import Miku.Templates.Log
  ( Heading
  , logHeadingL
  , logGoalsL
  , logTasksL
  , Log
  , readCurrentLog
  , showHeading
  , ongoingTask
  )

import Miku.UI.Draw.CurrentTask (drawCurrentTask)
import Miku.UI.Draw.Goals       (drawCompletedGoals, drawNotCompletedGoals)
import Miku.UI.Draw.StatusLine  (drawStatusLine)
import Miku.UI.State
  ( Action
  , AppState(AppState)
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , Keys
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
  handleEventState = handleAnyStateEvent

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
                    , drawCurrentTaskWindow s
                    , Core.padTop (Pad 1) $ Core.hBox
                        [ Core.padLeft (Pad 1) $ drawCompletedGoals True (s ^. clsLogL . logGoalsL)
                        , Core.padLeft (Pad 1) $ drawNotCompletedGoals False (s ^. clsLogL . logGoalsL)
                        ]
                    ]
      , drawStatusLine (Text.pack $ s ^. prevKeysL) ""
      ]
  ]

drawCurrentTaskWindow :: CurrentLogState -> Widget n
drawCurrentTaskWindow s =
    drawCurrentTask False
  $ maybeToRight "There' currently no ongoing task." (ongoingTask $ s ^. clsLogL)

drawHeading :: Heading -> Widget n
drawHeading h =
  Core.vBox [ Core.padAll 1 $ Core.hCenter $ Core.txt $ showHeading h
            ]
