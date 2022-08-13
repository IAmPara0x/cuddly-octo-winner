{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Miku.UI.Mode.CurrentLog
  ( clcConfigPathL
  , clsLogL
  , clsConfigL
  , clsCurrentWindow
  , CurrentLogConfig(..)
  , CurrentLogState(..)
  , toCurrentLogMode
  )
  where

import Brick.Main                  qualified as Brick
import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Brick.Types
  ( Widget
  , Padding(Pad)
  )

import Control.Lens (makeLenses, (^.), (.~))
import Data.Map             qualified as Map
import Data.Text            qualified as Text


import Miku.Templates.Log
  ( Heading
  , logHeadingL
  , logGoalsL
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


data Window = TopLeft     
            | TopRight    
            | BottomRight 
            | BottomLeft  
            deriving stock (Enum, Eq)
            
newtype CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL :: FilePath
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL       :: CurrentLogConfig
                  , _clsKeyMapL       :: KeyMap CurrentLogState
                  , _clsLogL          :: Log
                  , _clsPrevKeysL     :: Keys
                  , _clsCurrentWindow :: Window
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
                          , _clsCurrentWindow = TopLeft
                          }

currentLogStateActions :: KeyMap CurrentLogState
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               , ("<spc>wj", down)
               , ("<spc>wk", up)
               , ("<spc>wl", right)
               , ("<spc>wh", left)
               ]
  where

    exitApp :: Action CurrentLogState
    exitApp  = Brick.halt . AppState

    down :: Action CurrentLogState
    down clState =
      case clState ^. clsCurrentWindow of
        TopLeft  -> Brick.continue $ AppState (clState & clsCurrentWindow .~ BottomLeft)
        TopRight -> Brick.continue $ AppState (clState & clsCurrentWindow .~ BottomRight)
        _        -> Brick.continue $ AppState clState

    up :: Action CurrentLogState
    up clState =
      case clState ^. clsCurrentWindow of
        BottomLeft  -> Brick.continue $ AppState (clState & clsCurrentWindow .~ TopLeft)
        BottomRight -> Brick.continue $ AppState (clState & clsCurrentWindow .~ TopRight)
        _           -> Brick.continue $ AppState clState


    left :: Action CurrentLogState
    left clState =
      case clState ^. clsCurrentWindow of
        TopRight    -> Brick.continue $ AppState (clState & clsCurrentWindow .~ TopLeft)
        BottomRight -> Brick.continue $ AppState (clState & clsCurrentWindow .~ BottomLeft)
        _           -> Brick.continue $ AppState clState

    right :: Action CurrentLogState
    right clState =
      case clState ^. clsCurrentWindow of
        TopLeft    -> Brick.continue $ AppState (clState & clsCurrentWindow .~ TopRight)
        BottomLeft -> Brick.continue $ AppState (clState & clsCurrentWindow .~ BottomRight)
        _          -> Brick.continue $ AppState clState

drawCurrentLogState :: CurrentLogState -> [Widget n]
drawCurrentLogState clState =
  [ Core.vBox
      [ Core.vLimitPercent 94 $ drawAllWindows (map ($ clState) allWindows) clState
      , drawStatusLine (Text.pack $ clState ^. prevKeysL) ""
      ]
  ]
  where
    allWindows = [ drawCurrentTaskWindow 
                 , drawStatsWindow
                 , drawNotCompletedGoalsWindow
                 , drawCompletedGoalsWindow
                 ]


drawAllWindows :: [Widget n] -> CurrentLogState -> Widget n
drawAllWindows [topLeftWindow, topRightWindow, botRightWindow, botLeftWindow] clState =
          Core.vBox [ drawHeading (clState ^. clsLogL . logHeadingL)
                    , Core.hBox [topLeftWindow, topRightWindow]
                    , Core.padTop (Pad 1) $ Core.hBox
                        [ botLeftWindow
                        , botRightWindow
                        ]
                    ]
drawAllWindows _ _ = error "x_x"

drawHeading :: Heading -> Widget n
drawHeading = Core.padAll 1 . Core.hCenter . Core.txt . showHeading

drawCurrentTaskWindow :: CurrentLogState -> Widget n
drawCurrentTaskWindow clState
  | clState ^. clsCurrentWindow == TopLeft 
      = drawCurrentTask True
      $ maybeToRight "There' currently no ongoing task." (ongoingTask $ clState ^. clsLogL)
  | otherwise
      = drawCurrentTask False
      $ maybeToRight "There' currently no ongoing task." (ongoingTask $ clState ^. clsLogL)

drawStatsWindow :: CurrentLogState -> Widget n
drawStatsWindow clState
  | clState ^. clsCurrentWindow == TopRight
      = Core.withBorderStyle Border.unicodeRounded $ Border.border
      $ Core.center $ Core.txt "No Stats!"
  | otherwise
      = Core.center $ Core.txt "No Stats!"

drawNotCompletedGoalsWindow :: CurrentLogState -> Widget n
drawNotCompletedGoalsWindow clState
  | clState ^. clsCurrentWindow == BottomRight
      = Core.padLeft (Pad 1)
      $ drawNotCompletedGoals True
      $ clState ^. clsLogL . logGoalsL
  | otherwise
      = Core.padLeft (Pad 1)
      $ drawNotCompletedGoals False
      $ clState ^. clsLogL . logGoalsL

drawCompletedGoalsWindow :: CurrentLogState -> Widget n
drawCompletedGoalsWindow clState
  | clState ^. clsCurrentWindow == BottomLeft
      = Core.padLeft (Pad 1)
      $ drawCompletedGoals True
      $ clState ^. clsLogL . logGoalsL
  | otherwise
      = Core.padLeft (Pad 1)
      $ drawCompletedGoals False
      $ clState ^. clsLogL . logGoalsL


toCurrentLogMode :: forall a. IsMode a => Action a
toCurrentLogMode _ = liftIO (defState @CurrentLogState) >>= Brick.continue . AppState
