{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Miku.Mode.CurrentLog
  ( CurrentLog
  , currentLogStateActions
  )
  where

import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Center        qualified as Core
import Brick.Widgets.Core          qualified as Core

import Brick.Types
  ( Widget
  , Padding(Pad)
  )

import Control.Lens (makeLenses, (^.), (.~), _1, _2)
import Control.Monad.Trans.Reader (mapReader)
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
  , goalsNotDone
  )

import Miku.Draw             (W, Draw(..), draw)
import Miku.Draw.CurrentTask (CurrentTask(NoCurrentTask, CurrentTask))
import Miku.Draw.Goals       (CompletedGoals(..), NotCompletedGoals(..))
import Miku.Draw.StatusLine  (drawStatusLine)
import Miku.Mode
  ( Action
  , handleAnyStateEvent
  , haltAction
  , continueAction
  , gsModeStateL
  , gsPrevKeysL
  , IsMode(..)
  , KeyMap
  , Name
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
                   , _clcClockAnimTimeL :: Int
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL            :: CurrentLogConfig
                  , _clsCurrentWindowL     :: Window
                  , _clsLogL               :: Log
                  , _clsClockAnimStateL    :: Int
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState

-- clsClockTick :: CurrentLogState -> (Int, Int)
-- clsClockTick clState = clState ^. clsGlobalTickL
--                      & _2 .~ clState ^. clsConfigL . clcClockAnimTimeL

instance Default CurrentLogConfig where
  def = CurrentLogConfig
          { _clcConfigPathL = "/home/iamparadox/.miku/"
          , _clcClockAnimTimeL = 5
          }

instance IsMode CurrentLog where
  type ModeState CurrentLog = CurrentLogState

  defState         = ( , currentLogStateActions) <$> defCurrentLogState
  drawState        = drawCurrentLogState
  handleEventState = handleAnyStateEvent

defCurrentLogState :: IO CurrentLogState
defCurrentLogState =
  do

    elog <- runExceptT $ readCurrentLog (def ^. clcConfigPathL </> "logs")

    case elog of
      Left err  -> error err
      Right log -> return $ CurrentLogState
                          { _clsConfigL            = def
                          , _clsLogL               = log
                          , _clsCurrentWindowL     = (TopWindow, LeftWindow)
                          , _clsClockAnimStateL    = 0
                          }

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               , ("k", up)
               , ("j", down)
               , ("l", right)
               , ("h", left)
               ]
  where

    exitApp :: Action CurrentLog
    exitApp  = haltAction

    up, down, left, right :: Action CurrentLog
    up     = modify (gsModeStateL . clsCurrentWindowL . _1 .~ TopWindow) >> continueAction
    down   = modify (gsModeStateL . clsCurrentWindowL . _1 .~ BottomWindow) >> continueAction
    left   = modify (gsModeStateL . clsCurrentWindowL . _2 .~ LeftWindow) >> continueAction
    right  = modify (gsModeStateL . clsCurrentWindowL . _2 .~ RightWindow) >> continueAction


-- changeClockState :: CurrentLogState -> CurrentLogState
-- changeClockState clState
--   = clState & clsClockAnimStateL %~
--       bool id (\n -> mod (n + 1) 4) (uncurry rem (clsClockTick clState) == 0)


drawCurrentLogState :: DrawMode CurrentLog
drawCurrentLogState = do

  gstate <- ask

  let clState = gstate ^. gsModeStateL

      allWindows :: [Reader CurrentLogState (Widget Name)]
      allWindows = [ mapReader draw drawHeading
                   , mapReader draw drawCurrentTaskWindow
                   , mapReader draw drawStatsWindow
                   , mapReader draw drawNotCompletedGoalsWindow
                   , mapReader draw drawCompletedGoalsWindow
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

      windows = runReader (sequence allWindows) clState

  return [ Core.vBox
            [ Core.vLimitPercent 94 $ drawAllWindows windows
            , drawStatusLine (Text.pack $ gstate ^. gsPrevKeysL @CurrentLog) ""
            ]
         ]

drawHeading :: W CurrentLog (Widget Name)
drawHeading = do
  s <- ask

  let heading = s ^. clsLogL . logHeadingL

  return $ Draw { _focusedL  = False
                , _drawableL = Core.padAll 1
                             $ Core.hCenter
                             $ Core.txt
                             $ showHeading heading
                }

drawCurrentTaskWindow :: W CurrentLog CurrentTask
drawCurrentTaskWindow = do

  mstate <- ask
  isFocus <- reader $ checkWindowPos (TopWindow, LeftWindow)

  let widget = case ongoingTask (mstate ^. clsLogL) of
                  Nothing     -> NoCurrentTask "There's currently no ongoing task."
                  (Just task) -> CurrentTask (mstate ^. clsClockAnimStateL) task

  return $ Draw { _focusedL = isFocus, _drawableL = widget}

  
drawStatsWindow :: W CurrentLog (Widget Name)
drawStatsWindow = do
  isFocus <- reader $ checkWindowPos (TopWindow, RightWindow)
  return $ Draw { _focusedL = isFocus, _drawableL = Border.border $ Core.center $ Core.txt "No Stats!"}


drawNotCompletedGoalsWindow :: W CurrentLog NotCompletedGoals
drawNotCompletedGoalsWindow = do

    isFocus <- reader $ checkWindowPos (BottomWindow, RightWindow)
    widget <- reader $ NotCompletedGoals . goalsNotDone . (^. clsLogL . logGoalsL)

    return $ Draw { _focusedL = isFocus, _drawableL = widget}

drawCompletedGoalsWindow :: W CurrentLog CompletedGoals
drawCompletedGoalsWindow = do

  isFocus <- reader $ checkWindowPos (BottomWindow, LeftWindow)
  widget <- reader $ CompletedGoals . goalsNotDone . (^. clsLogL . logGoalsL)

  return $ Draw { _focusedL = isFocus, _drawableL = widget}

checkWindowPos :: Window -> CurrentLogState -> Bool
checkWindowPos window clState = window == clState ^. clsCurrentWindowL
