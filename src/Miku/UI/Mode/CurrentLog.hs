{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Miku.UI.Mode.CurrentLog
  ( CurrentLog
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

import Control.Lens (makeLenses, (^.), (.~), (%~), (+~), _1, _2, Lens', lens)
import Data.Default (Default(def))
import Data.Map             qualified as Map
import Data.Text            qualified as Text


import Miku.Templates.Log
  ( logHeadingL
  , logGoalsL
  , Log
  , readCurrentLog
  , showHeading
  , ongoingTask
  , goalsDone
  , goalsNotDone
  )

import Miku.UI.Draw             (Draw, drawWidget)
import Miku.UI.Draw.CurrentTask (CurrentTask(NoCurrentTask, CurrentTask))
import Miku.UI.Draw.Goals       (CompletedGoals(..), NotCompletedGoals(..))
import Miku.UI.Draw.StatusLine  (drawStatusLine)
import Miku.UI.State
  ( Action
  , AppState(AppState)
  , handleAnyStateEvent
  , IsMode(..)
  , KeyMap
  , Keys
  , Name
  , Tick(Tick)
  , DrawMode
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

data CurrentLog

data CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL    :: FilePath
                   , _clcClearKeysTimeL :: Int
                   , _clcMaxTicksCountL :: Int
                   , _clcClockAnimTimeL :: Int
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL            :: CurrentLogConfig
                  , _clsCurrentWindowL     :: Window
                  , _clsKeyMapL            :: KeyMap CurrentLog
                  , _clsLogL               :: Log
                  , _clsPrevKeysL          :: Keys
                  , _clsGlobalTickCounterL :: Int
                  , _clsKeysTickCounterL   :: Int
                  , _clsClockAnimStateL    :: Int
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState

clsKeysTickL :: Lens' CurrentLogState (Int, Int)
clsKeysTickL  = lens getter setter
  where
    getter clState
        = (clState ^. clsKeysTickCounterL, clState ^. clsConfigL . clcClearKeysTimeL)
    setter clState (val, conf)
        = clState & clsKeysTickCounterL .~ val
                  & clsConfigL . clcClearKeysTimeL .~ conf

clsGlobalTickL :: Lens' CurrentLogState (Int, Int)
clsGlobalTickL  = lens getter setter
  where
    getter clState
        = (clState ^. clsGlobalTickCounterL, clState ^. clsConfigL . clcMaxTicksCountL)
    setter clState (val, conf)
        = clState & clsGlobalTickCounterL .~ val
                  & clsConfigL . clcMaxTicksCountL .~ conf

clsClockTick :: CurrentLogState -> (Int, Int)
clsClockTick clState = clState ^. clsGlobalTickL
                     & _2 .~ clState ^. clsConfigL . clcClockAnimTimeL

instance Default CurrentLogConfig where
  def = CurrentLogConfig
          { _clcConfigPathL = "/home/iamparadox/.miku/"
          , _clcClearKeysTimeL = 5
          , _clcMaxTicksCountL = 100
          , _clcClockAnimTimeL = 5
          }

instance IsMode CurrentLog where
  type ModeState CurrentLog = CurrentLogState

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
    = Brick.continue
    $ AppState @CurrentLog Proxy $ updateTickCounter clState
    & id %~ clearPrevKeys
    & id %~ changeClockState
handleCurrentLogEvent clState event
    = handleAnyStateEvent @CurrentLog (clState & clsKeysTickCounterL .~ 0) event

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               , ("j", down)
               , ("k", up)
               , ("l", right)
               , ("h", left)
               ]
  where

    exitApp :: Action CurrentLog
    exitApp  = ask >>= lift . Brick.halt

    up, down, left, right :: Action CurrentLog
    up     = ask >>= lift . Brick.continue . (clsCurrentWindowL . _1 .~ TopWindow)
    down   = ask >>= lift . Brick.continue . (clsCurrentWindowL . _1 .~ BottomWindow)
    left   = ask >>= lift . Brick.continue . (clsCurrentWindowL . _2 .~ LeftWindow)
    right  = ask >>= lift . Brick.continue . (clsCurrentWindowL . _2 .~ RightWindow)


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


drawCurrentLogState :: DrawMode CurrentLog
drawCurrentLogState = do

  clState <- ask

  let allWindows = [ drawHeading
                   , drawCurrentTaskWindow
                   , drawStatsWindow
                   , drawNotCompletedGoalsWindow
                   , drawCompletedGoalsWindow
                   ]
      
      drawAllWindows :: [Widget Name] -> Widget Name
      drawAllWindows [heading, topLeftWindow, topRightWindow, botRightWindow, botLeftWindow] =
                Core.vBox [ heading
                          , Core.hBox [topLeftWindow, topRightWindow]
                          , Core.padTop (Pad 1) $ Core.hBox
                              [ botLeftWindow
                              , botRightWindow
                              ]
                          ]
      drawAllWindows _ = error "x_x"

  windows <- sequence allWindows

  return [ Core.vBox
            [ Core.vLimitPercent 94 $ drawAllWindows windows
            , drawStatusLine (Text.pack $ clState ^. prevKeysL @CurrentLog) ""
            ]
         ]

drawHeading :: Draw CurrentLog
drawHeading = do
  s <- ask

  let heading = s ^. clsLogL . logHeadingL

  return $ Core.padAll 1
         $ Core.hCenter
         $ Core.txt
         $ showHeading heading

drawCurrentTaskWindow :: Draw CurrentLog
drawCurrentTaskWindow = do

  mstate <- ask

  let isFocus :: Reader CurrentLogState Bool
      isFocus = reader $ checkWindowPos (TopWindow, LeftWindow)

      draw :: Reader CurrentLogState CurrentTask
      draw = case ongoingTask (mstate ^. clsLogL) of
                Nothing -> return $ NoCurrentTask "There's currently no ongoing task."
                (Just task) -> return $ CurrentTask (mstate ^. clsClockAnimStateL) task

  drawWidget @CurrentLog isFocus draw
  
drawStatsWindow :: Draw CurrentLog
drawStatsWindow = do
  s <- ask

  if checkWindowPos (TopWindow, RightWindow) s
    then return $ Core.withBorderStyle Border.unicodeRounded
                $ Border.border
                $ Core.center
                $ Core.txt "No Stats!"
    else return $ Core.center
                $ Core.txt "No Stats!"

drawNotCompletedGoalsWindow :: Draw CurrentLog
drawNotCompletedGoalsWindow = do

    let isFocus :: Reader CurrentLogState Bool
        isFocus = reader $ checkWindowPos (BottomWindow, RightWindow)

        draw :: Reader CurrentLogState NotCompletedGoals
        draw = reader $ NotCompletedGoals . goalsNotDone . (^. clsLogL . logGoalsL)

    drawWidget @CurrentLog isFocus draw

drawCompletedGoalsWindow :: Draw CurrentLog
drawCompletedGoalsWindow = do

  let isFocus :: Reader CurrentLogState Bool
      isFocus = reader $ checkWindowPos (BottomWindow, LeftWindow)

      draw :: Reader CurrentLogState CompletedGoals
      draw = reader $ CompletedGoals . goalsDone . (^. clsLogL . logGoalsL)

  drawWidget @CurrentLog isFocus draw

checkWindowPos :: Window -> CurrentLogState -> Bool
checkWindowPos window clState = window == clState ^. clsCurrentWindowL
