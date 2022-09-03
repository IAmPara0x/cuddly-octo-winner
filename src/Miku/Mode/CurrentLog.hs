{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Miku.Mode.CurrentLog
  ( CurrentLog
  , currentLogStateActions
  )
  where

import Brick.Widgets.Border        qualified as Border
import Brick.Widgets.Border.Style  qualified as Border
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

import Miku.Draw             (W, Draw(..), draw, defDraw, focusedL, Drawable)
import Miku.Draw.CurrentTask (CurrentTask(NoCurrentTask, CurrentTask))
import Miku.Draw.Goals       (CompletedGoals(..), NotCompletedGoals(..))
import Miku.Draw.StatusLine  (StatusLine(..))
import Miku.Mode
  ( Action
  , handleAnyStateEvent
  , haltAction
  , continueAction
  , GlobalState
  , gsModeStateL
  , gsEditingModeL
  , IsMode(..)
  , KeyMap
  , Name
  , DrawMode
  )

import System.FilePath ((</>))

import Relude


data VerticalPos = TopWindow
                 | BotWindow
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
                  , _clsCompletedGoalsL    :: (Window, Draw CompletedGoals)
                  , _clsNotCompletedGoalsL :: (Window, Draw NotCompletedGoals)
                  , _clsCurrentTaskL       :: (Window, Draw CurrentTask)
                  , _clsStatsL             :: (Window, Draw (Widget Name)) -- NOTE: This is temp
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState

-- clsWindowsL :: Current

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

    let log = either error id elog

        completedGoals    = defDraw $ CompletedGoals 0 $ goalsDone $ log  ^. logGoalsL
        notCompletedGoals = defDraw $ NotCompletedGoals 0 $ goalsNotDone $ log  ^. logGoalsL
        currentTask       = defDraw $ maybe (NoCurrentTask "There's currently no ongoing task.") 
                                            (CurrentTask 0) $ ongoingTask log
        stats             = defDraw $ Border.border $ Core.center $ Core.txt "No Stats!"

    return $ CurrentLogState
              { _clsConfigL            = def
              , _clsLogL               = log
              , _clsCurrentWindowL     = (TopWindow, LeftWindow)
              , _clsClockAnimStateL    = 0
              , _clsCompletedGoalsL    = ((BotWindow, LeftWindow), completedGoals)
              , _clsNotCompletedGoalsL = ((BotWindow, RightWindow), notCompletedGoals)
              , _clsCurrentTaskL       = ((TopWindow, LeftWindow), currentTask & focusedL .~ True)
              , _clsStatsL             = ((TopWindow, RightWindow), stats)
              }

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               , ("k", up)
               , ("j", down)
               , ("l", right)
               , ("h", left)
               , ("<tab>", incAction)
               ]
  where

    exitApp :: Action CurrentLog
    exitApp  = haltAction

    up, down, left, right :: Action CurrentLog
    up     = modify (gsModeStateL . clsCurrentWindowL . _1 .~ TopWindow) >> continueAction
    down   = modify (gsModeStateL . clsCurrentWindowL . _1 .~ BotWindow) >> continueAction
    left   = modify (gsModeStateL . clsCurrentWindowL . _2 .~ LeftWindow) >> continueAction
    right  = modify (gsModeStateL . clsCurrentWindowL . _2 .~ RightWindow) >> continueAction


    incAction = do
      gstate <- get

      return undefined


drawCurrentLogState :: DrawMode CurrentLog
drawCurrentLogState = do

  gstate <- ask

  let allWindows :: [Reader (GlobalState CurrentLog) (Widget Name)]
      allWindows = [ mapReader draw heading
                   , mapReader draw currentTaskWindow
                   , mapReader draw statsWindow
                   , mapReader draw notCompletedGoalsWindow
                   , mapReader draw completedGoalsWindow
                   ]

      drawAllWindows :: [Widget Name] -> Widget Name
      drawAllWindows [headingWindow, topLeftWindow, topRightWindow, botRightWindow, botLeftWindow] =
                Core.vBox [ headingWindow
                          , Core.hBox [topLeftWindow, topRightWindow]
                          , Core.padTop (Pad 1) $ Core.hBox
                              [ botLeftWindow
                              , botRightWindow
                              ]
                          ]
      drawAllWindows _ = error "x_x"

  windows <- sequence allWindows

  return [ Core.vBox
            [ drawAllWindows windows
            , draw $ runReader statusLine gstate
            ]
         ]

heading :: W CurrentLog (Widget Name)
heading = do
  mstate <-  (^. gsModeStateL) <$> ask

  let logHeading = mstate ^. clsLogL . logHeadingL
      widget  = Core.padAll 1 $ Core.hCenter $ Core.txt $ showHeading logHeading

  return $ Draw { _focusedL    = False
                , _drawableL   = widget
                , _borderTypeL = bordertype False
                }

currentTaskWindow :: W CurrentLog CurrentTask
currentTaskWindow = do

  mstate <- (^. gsModeStateL ) <$> ask

  let isFocus = checkWindowPos (TopWindow, LeftWindow) mstate
      widget = case ongoingTask (mstate ^. clsLogL) of
                  Nothing     -> NoCurrentTask "There's currently no ongoing task."
                  (Just task) -> CurrentTask (mstate ^. clsClockAnimStateL) task

  return $ Draw { _focusedL = isFocus, _drawableL = widget, _borderTypeL = bordertype isFocus }


statsWindow :: W CurrentLog (Widget Name)
statsWindow = do
  mstate <- (^. gsModeStateL) <$> ask

  let isFocus = checkWindowPos (TopWindow, RightWindow) mstate
      widget = Border.border $ Core.center $ Core.txt "No Stats!"

  return $ Draw { _focusedL = isFocus, _drawableL = widget, _borderTypeL = bordertype isFocus }


notCompletedGoalsWindow :: W CurrentLog NotCompletedGoals
notCompletedGoalsWindow = do

  mstate <- (^. gsModeStateL) <$> ask

  let isFocus = checkWindowPos (BotWindow, RightWindow) mstate
      widget  = NotCompletedGoals 0 $ goalsNotDone (mstate ^. clsLogL . logGoalsL)

  return $ Draw { _focusedL = isFocus, _drawableL = widget, _borderTypeL = bordertype isFocus }

completedGoalsWindow :: W CurrentLog CompletedGoals
completedGoalsWindow = do

  mstate <- (^. gsModeStateL) <$> ask

  let isFocus = checkWindowPos (BotWindow, LeftWindow) mstate
      widget  = CompletedGoals 1 $ goalsDone $ mstate ^. clsLogL . logGoalsL

  return $ Draw { _focusedL = isFocus, _drawableL = widget, _borderTypeL = bordertype isFocus }

statusLine :: W CurrentLog StatusLine
statusLine = do

  gstate <- ask

  let mstate = gstate ^. gsModeStateL
      currWindow = case mstate ^. clsCurrentWindowL of
                     (TopWindow, LeftWindow)  -> ["OngoingTask"]
                     (TopWindow, RightWindow) -> ["Stats"]
                     (BotWindow, LeftWindow)  -> ["TODO", "Completed"]
                     (BotWindow, RightWindow) -> ["TODO", "NotCompleted"]

      widget = StatusLine { _slEditingModeL = gstate ^. gsEditingModeL
                          , _slModeNameL = "CurrentLog"
                          , _slOtherInfoL = currWindow
                          }
  return $ Draw { _focusedL = False, _drawableL = widget, _borderTypeL = Border.unicode }


-- Helpers
checkWindowPos :: Window -> CurrentLogState -> Bool
checkWindowPos window clState = window == clState ^. clsCurrentWindowL

bordertype :: Bool -> Border.BorderStyle
bordertype True  = Border.unicodeRounded
bordertype False = Border.borderStyleFromChar ' '

changeFocus :: Window -> [(Window, Draw a)] -> [(Window, Draw a)]
changeFocus _window []        = []
changeFocus window ((w,x):xs)
  | window == w = (w,x & focusedL .~ True)  : changeFocus window xs
  | otherwise   = (w,x & focusedL .~ False) : changeFocus window xs
