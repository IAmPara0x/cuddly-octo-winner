{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Miku.UI.Mode.CurrentLog
  ( clcConfigPathL
  , clsLogL
  , clsConfigL
  , clsCurrentWindowL
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
  , BrickEvent(AppEvent)
  , Padding(Pad)
  , EventM
  , Next
  )

import Control.Lens (makeLenses, (^.), (.~), (%~), (+~), _1, _2, Lens', lens, both)
import Data.Default (Default(def))
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
  , goalsDone
  , goalsNotDone
  )

import Miku.UI.Draw             (mkDraw)
import Miku.UI.Draw.CurrentTask (CurrentTask(NoCurrentTask, CurrentTask))
import Miku.UI.Draw.Goals       (CompletedGoals(..), NotCompletedGoals(..))
import Miku.UI.Draw.StatusLine  (drawStatusLine)
import Miku.UI.State
  ( Action
  , AppState(AppState)
  , continue
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick(Tick)
  )

import System.FilePath ((</>))

import Relude


data VerticalPos = TopWindow
                 | BottomWindow
                 deriving stock (Eq)

data HorizontalPos = LeftWindow
                   | RightWindow
                   deriving stock (Eq)

type Window = (VerticalPos, HorizontalPos)

data CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL    :: FilePath
                   , _clcClearKeysTimeL :: Integer
                   , _clcMaxTicksCountL :: Integer
                   , _clcClockAnimTimeL :: Int
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL            :: CurrentLogConfig
                  , _clsCurrentWindowL     :: Window
                  , _clsKeyMapL            :: KeyMap CurrentLogState
                  , _clsLogL               :: Log
                  , _clsPrevKeysL          :: Keys
                  , _clsGlobalTickCounterL :: Integer
                  , _clsKeysTickCounterL   :: Integer
                  , _clsClockAnimStateL    :: Int
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState

clsKeysTickL :: Lens' CurrentLogState (Integer, Integer)
clsKeysTickL  = lens getter setter
  where
    getter clState
        = (clState ^. clsKeysTickCounterL, clState ^. clsConfigL . clcClearKeysTimeL)
    setter clState (val, conf)
        = clState & clsKeysTickCounterL .~ val
                  & clsConfigL . clcClearKeysTimeL .~ conf

clsGlobalTickL :: Lens' CurrentLogState (Integer, Integer)
clsGlobalTickL  = lens getter setter
  where
    getter clState
        = (clState ^. clsGlobalTickCounterL, clState ^. clsConfigL . clcMaxTicksCountL)
    setter clState (val, conf)
        = clState & clsGlobalTickCounterL .~ val
                  & clsConfigL . clcMaxTicksCountL .~ conf

clsClockTick :: CurrentLogState -> (Int, Int)
clsClockTick clState = clState ^. clsGlobalTickL
                     & both %~ fromInteger
                     & _2 .~ clState ^. clsConfigL . clcClockAnimTimeL

instance Default CurrentLogConfig where
  def = CurrentLogConfig
          { _clcConfigPathL = "/home/iamparadox/.miku/"
          , _clcClearKeysTimeL = 5
          , _clcMaxTicksCountL = 100
          , _clcClockAnimTimeL = 5
          }

instance IsMode CurrentLogState where

  defState         = defCurrentLogState
  drawState        = drawCurrentLogState
  handleEventState = handleCurrentLogEvent

  keyMapL          = clsKeyMapL
  prevKeysL        = clsPrevKeysL


defCurrentLogState :: IO CurrentLogState
defCurrentLogState =
  do

    elog <- runExceptT $ readCurrentLog (def ^. clcConfigPathL </> "logs")

    case elog of
      Left err  -> error err
      Right log -> return $ CurrentLogState
                          { _clsConfigL            = def
                          , _clsKeyMapL            = currentLogStateActions
                          , _clsLogL               = log
                          , _clsPrevKeysL          = []
                          , _clsCurrentWindowL     = (TopWindow, LeftWindow)
                          , _clsGlobalTickCounterL = 0
                          , _clsKeysTickCounterL   = 0
                          , _clsClockAnimStateL    = 0
                          }

handleCurrentLogEvent :: CurrentLogState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleCurrentLogEvent clState (AppEvent Tick)
    = continue $ updateTickCounter clState
               & id %~ clearPrevKeys
               & id %~ changeClockState
handleCurrentLogEvent clState event
    = handleAnyStateEvent (clState & clsKeysTickCounterL .~ 0) event

currentLogStateActions :: KeyMap CurrentLogState
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               , ("j", down)
               , ("k", up)
               , ("l", right)
               , ("h", left)
               ]
  where

    exitApp :: Action CurrentLogState
    exitApp  = Brick.halt . AppState

    up, down, left, right :: Action CurrentLogState
    down  clState  = continue $ clState & clsCurrentWindowL . _1 .~ BottomWindow
    up    clState  = continue $ clState & clsCurrentWindowL . _1 .~ TopWindow
    left  clState  = continue $ clState & clsCurrentWindowL . _2 .~ LeftWindow
    right clState  = continue $ clState & clsCurrentWindowL . _2 .~ RightWindow


clearPrevKeys :: CurrentLogState -> CurrentLogState
clearPrevKeys clState
  = clState & clsPrevKeysL %~
      bool id (const []) (uncurry rem (clState ^. clsKeysTickL) == 0)

updateTickCounter :: CurrentLogState -> CurrentLogState
updateTickCounter clState =
  clState & clsGlobalTickCounterL .~ uncurry mod
              (clState ^. clsGlobalTickL & _1 +~ 1)
          & clsKeysTickCounterL   .~ uncurry mod
              (clState ^. clsKeysTickL & _1 +~ 1 & _2 .~ clState ^. clsGlobalTickL . _2)


changeClockState :: CurrentLogState -> CurrentLogState
changeClockState clState
  = clState & clsClockAnimStateL %~
      bool id (\n -> mod (n + 1) 4) (uncurry rem (clsClockTick clState) == 0)


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
drawCurrentTaskWindow =
  mkDraw (checkWindowPos (TopWindow, LeftWindow))
         (\s -> maybe (NoCurrentTask "There's currently no ongoing task.")
                      (CurrentTask (s ^. clsClockAnimStateL)) $ ongoingTask (s ^. clsLogL))

drawStatsWindow :: CurrentLogState -> Widget n
drawStatsWindow clState
  | checkWindowPos (TopWindow, RightWindow) clState
      = Core.withBorderStyle Border.unicodeRounded $ Border.border
      $ Core.center $ Core.txt "No Stats!"
  | otherwise
      = Core.center $ Core.txt "No Stats!"

drawNotCompletedGoalsWindow :: CurrentLogState -> Widget n
drawNotCompletedGoalsWindow = Core.padLeft (Pad 1)
                            . mkDraw (checkWindowPos (BottomWindow, RightWindow))
                                     (NotCompletedGoals . goalsNotDone . (^. clsLogL . logGoalsL))

drawCompletedGoalsWindow :: CurrentLogState -> Widget n
drawCompletedGoalsWindow = mkDraw (checkWindowPos (BottomWindow, LeftWindow))
                                  (CompletedGoals . goalsDone . (^. clsLogL . logGoalsL))

toCurrentLogMode :: forall a. IsMode a => Action a
toCurrentLogMode _ = liftIO (defState @CurrentLogState) >>= Brick.continue . AppState

checkWindowPos :: Window -> CurrentLogState -> Bool
checkWindowPos window clState = window == clState ^. clsCurrentWindowL
