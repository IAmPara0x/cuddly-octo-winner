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

import Miku.UI.Draw             (drawWidget)
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
    = Brick.continue
    $ AppState $ updateTickCounter clState
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
    exitApp  = ask >>= lift . Brick.halt

    up, down, left, right :: Action CurrentLogState
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


drawCurrentLogState :: CurrentLogState -> [Widget n]
drawCurrentLogState clState =
  [ Core.vBox
      [ Core.vLimitPercent 94 $ drawAllWindows $ runReader (sequence allWindows) clState
      , drawStatusLine (Text.pack $ clState ^. prevKeysL) ""
      ]
  ]
  where
    allWindows = [ drawHeading
                 , drawCurrentTaskWindow
                 , drawStatsWindow
                 , drawNotCompletedGoalsWindow
                 , drawCompletedGoalsWindow
                 ]


drawAllWindows :: [Widget n] -> Widget n
drawAllWindows [heading, topLeftWindow, topRightWindow, botRightWindow, botLeftWindow] =
          Core.vBox [ heading
                    , Core.hBox [topLeftWindow, topRightWindow]
                    , Core.padTop (Pad 1) $ Core.hBox
                        [ botLeftWindow
                        , botRightWindow
                        ]
                    ]
drawAllWindows _ = error "x_x"

drawHeading :: Reader CurrentLogState (Widget n)
drawHeading = do
  s <- ask

  let heading = s ^. clsLogL . logHeadingL

  return $ Core.padAll 1
         $ Core.hCenter
         $ Core.txt
         $ showHeading heading

drawCurrentTaskWindow :: Reader CurrentLogState (Widget n)
drawCurrentTaskWindow = do

  let isFocus :: Reader CurrentLogState Bool
      isFocus = checkWindowPos (TopWindow, LeftWindow) <$> ask

      draw :: Reader CurrentLogState CurrentTask
      draw = do
        s <- ask
        case ongoingTask (s ^. clsLogL) of
          Nothing -> return $ NoCurrentTask "There's currently no ongoing task."
          (Just task) -> return $ CurrentTask (s ^. clsClockAnimStateL) task

  drawWidget isFocus draw
  
drawStatsWindow :: Reader CurrentLogState (Widget n)
drawStatsWindow = do
  s <- ask

  if checkWindowPos (TopWindow, RightWindow) s
    then return $ Core.withBorderStyle Border.unicodeRounded
                $ Border.border
                $ Core.center
                $ Core.txt "No Stats!"
    else return $ Core.center
                $ Core.txt "No Stats!"

drawNotCompletedGoalsWindow :: Reader CurrentLogState (Widget n)
drawNotCompletedGoalsWindow = do

    let isFocus :: Reader CurrentLogState Bool
        isFocus = checkWindowPos (BottomWindow, RightWindow) <$> ask

        draw :: Reader CurrentLogState NotCompletedGoals
        draw = NotCompletedGoals . goalsNotDone . (^. clsLogL . logGoalsL) <$> ask

    drawWidget isFocus draw

drawCompletedGoalsWindow :: Reader CurrentLogState (Widget n)
drawCompletedGoalsWindow = do

  let isFocus :: Reader CurrentLogState Bool
      isFocus = checkWindowPos (BottomWindow, LeftWindow) <$> ask

      draw :: Reader CurrentLogState CompletedGoals
      draw = CompletedGoals . goalsDone . (^. clsLogL . logGoalsL) <$> ask

  drawWidget isFocus draw

checkWindowPos :: Window -> CurrentLogState -> Bool
checkWindowPos window clState = window == clState ^. clsCurrentWindowL
