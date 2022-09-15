{-# LANGUAGE UndecidableInstances #-}
module Miku.Mode.CurrentLog
  ( CurrentLog
  , currentLogStateActions
  ) where

import Brick.Widgets.Border       qualified as Border
import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Center       qualified as Core
import Brick.Widgets.Core         qualified as Core

import Brick.Types                (Padding (Pad), Widget)

import Control.Lens               (makeLenses, (%~), (.~), (^.))
import Control.Monad.Trans.Reader (mapReader)
import Data.Default               (Default (def))
import Data.Map                   qualified as Map

import Miku.Templates.Log         (Log, logHeadingL, logTodosL, ongoingTask,
                                   readCurrentLog, showHeading, todosDone,
                                   todosNotDone)

import Miku.Draw                  (Draw (..), Drawable, W, borderTypeL, defDraw,
                                   draw, focusedL)
import Miku.Draw.CurrentTask      (CurrentTask (CurrentTask, NoCurrentTask),
                                   CurrentTaskItem (TaskName),
                                   changeCurrentTaskFocus)
import Miku.Draw.StatusLine       (StatusInfo (..), StatusLine (..),
                                   StatusLineInfo (..))
import Miku.Draw.Todos            (Completed, NotCompleted, Todos,
                                   changeTodoIdx, mkCompletedTodos,
                                   mkNotCompletedTodos)
import Miku.Events                (continueAction, haltAction,
                                   handleAnyStateEvent, toNormalMode)
import Miku.Mode                  (Action, DrawMode, GlobalState, IsMode (..),
                                   KeyMap (..), Name, gsEditingModeL,
                                   gsModeStateL)

import Miku.Editing               (EditingMode (..))
import Miku.Mode.Utility          (Window, horizMove, vertMove, viewWindow,
                                   window)

import Miku.Types.Rec             (Field (rmodify), Rec (..), recToList,
                                   rfilterMap, rfmap, rmap)
import System.FilePath            ((</>))

import Relude

-- TODO: Remove all errors and replace them with EitherT or Freer Monad

-- TODO: remove this
newtype Stats
  = Stats (Widget Name)
  deriving newtype (Drawable)

instance StatusLineInfo Stats where
  statusLineInfo _ = ["Stats"]

type WindowStates = '[CurrentTask, Stats, Todos NotCompleted, Todos Completed]

data CurrentLog
  = CurrentLog
  deriving stock (Show)

data CurrentLogConfig
  = CurrentLogConfig
      { _clcConfigPathL    :: FilePath
      , _clcClockAnimTimeL :: Int
      }

data CurrentLogState
  = CurrentLogState
      { _clsConfigL         :: CurrentLogConfig
      , _clsWindowL         :: Window 2 2
      , _clsLogL            :: Log
      , _clsClockAnimStateL :: Int
      , _clsAllWindowsL     :: Rec '[Drawable, StatusLineInfo] WindowStates Draw
      }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState


instance Default CurrentLogConfig where
  def = CurrentLogConfig
          { _clcConfigPathL = "/home/iamparadox/.miku/"
          , _clcClockAnimTimeL = 5
          }

instance StatusLineInfo CurrentLog where
  statusLineInfo x = [show x]

instance IsMode CurrentLog where
  type ModeState CurrentLog = CurrentLogState

  defState         = ( , currentLogStateActions) . switchWindow (vertMove 1) <$> defCurrentLogState
  drawState        = drawCurrentLogState
  handleEventState = handleAnyStateEvent


defCurrentLogState :: IO CurrentLogState
defCurrentLogState =
  do

    elog <- runExceptT $ readCurrentLog (def ^. clcConfigPathL </> "logs")

    let log = either error id elog

        completedTodos    = defDraw $ mkCompletedTodos 0 $ todosDone $ log  ^. logTodosL

        notCompletedTodos = defDraw $ mkNotCompletedTodos 0 $ todosNotDone $ log  ^. logTodosL

        currentTask       = defDraw $ maybe (NoCurrentTask "There's currently no ongoing task.")
                                            (CurrentTask TaskName) $ ongoingTask log

        stats             = defDraw $ Stats $ Border.border $ Core.center $ Core.txt "No Stats!"

    return $ CurrentLogState
              { _clsConfigL            = def
              , _clsWindowL            = window 0 0
              , _clsLogL               = log
              , _clsClockAnimStateL    = 0
              , _clsAllWindowsL        = currentTask :> stats :> notCompletedTodos :> completedTodos :> RNil
              }

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions = KeyMap { _normalModeMapL = normalKeyMap
                                , _insertModeMapL = Map.fromList [("jk", toNormalMode)]
                                }

  where

    normalKeyMap =
      Map.fromList [ ("q", haltAction)
                   , ("k", up)
                   , ("j", down)
                   , ("l", right)
                   , ("h", left)
                   , ("<tab>", incAction)
                   -- , ("<shift>+<tab>", decAction)
                   ]

    right,left,up,down :: Action 'Normal CurrentLog
    right = modify (gsModeStateL %~ switchWindow (horizMove 1)) >> continueAction
    left  = modify (gsModeStateL %~ switchWindow (horizMove (-1))) >> continueAction
    up    = modify (gsModeStateL %~ switchWindow (vertMove 1)) >> continueAction
    down  = modify (gsModeStateL %~ switchWindow (vertMove (-1))) >> continueAction

    incAction :: Action 'Normal CurrentLog
    incAction = modify (gsModeStateL . clsAllWindowsL %~ inc) >> continueAction
      where
        inc = rfmap _focusedL $ Identity (changeCurrentTaskFocus 1)
                             :> Identity id
                             :> Identity (changeTodoIdx 1)
                             :> Identity (changeTodoIdx 1)
                             :> RNil


drawCurrentLogState :: DrawMode emode CurrentLog
drawCurrentLogState = do

  gstate <- ask
  CurrentLogState{..} <- (^. gsModeStateL) <$> ask

  h <- mapReader draw heading
  let allWindows :: [Widget Name]
      allWindows = h : recToList draw _clsAllWindowsL

      drawAllWindows :: [Widget Name] -> Widget Name
      drawAllWindows [headingWindow, topWLeftindow, topWRightindow, botWLeftindow, botWRightindow] =
                Core.vBox [ headingWindow
                          , Core.hBox [topWLeftindow, topWRightindow]
                          , Core.padTop (Pad 1) $ Core.hBox
                              [ botWLeftindow
                              , botWRightindow
                              ]
                          ]
      drawAllWindows _ = error "x_x"

  return [ Core.vBox
            [ drawAllWindows allWindows
            , draw $ runReader statusLine gstate
            ]
         ]

heading :: W emode CurrentLog (Widget Name)
heading = do
  mstate <-  (^. gsModeStateL) <$> ask

  let logHeading = mstate ^. clsLogL . logHeadingL
      widget  = Core.padAll 1 $ Core.hCenter $ Core.txt $ showHeading logHeading

  return $ Draw { _focusedL    = False
                , _drawableL   = widget
                , _borderTypeL = Border.borderStyleFromChar ' '
                }

statusLine :: W emode CurrentLog (StatusLine emode)
statusLine = do

  (gstate :: GlobalState emode CurrentLog) <- ask
  CurrentLogState{..} <- (^. gsModeStateL) <$> ask

  let widget :: StatusLine emode
      widget = StatusLine { _slEditingModeL = gstate ^. gsEditingModeL
                          , _slInfoL = [ StatusInfo CurrentLog
                                       , windowStatusInfo
                                       ]
                          }

      windowStatusInfo = case rfilterMap _focusedL (StatusInfo . _drawableL) _clsAllWindowsL of
                          [x] -> x
                          _   -> error "There must be atleast one window focued"

  return $ Draw { _focusedL = False, _drawableL = widget, _borderTypeL = Border.unicode }

-- Helpers

switchWindow :: (Window 2 2 -> Window 2 2) -> CurrentLogState -> CurrentLogState
switchWindow f x = x & clsWindowL %~ f & changeFocus
  where

    changeFocus :: CurrentLogState -> CurrentLogState
    changeFocus mstate =
      case viewWindow (mstate ^. clsWindowL) of
        (0,0) -> mstate & clsAllWindowsL %~ rmodify @(Todos NotCompleted) focused . rmap notfocused
        (1,0) -> mstate & clsAllWindowsL %~ rmodify @(Todos Completed) focused . rmap notfocused
        (0,1) -> mstate & clsAllWindowsL %~ rmodify @CurrentTask focused . rmap notfocused
        (1,1) -> mstate & clsAllWindowsL %~ rmodify @Stats focused . rmap notfocused
        a     -> error $ "Window of coord " <> show a <> " is not possible!"


    focused :: Draw a -> Draw a
    focused a = a & focusedL .~ True & borderTypeL .~ Border.unicodeRounded

    notfocused :: Draw a -> Draw a
    notfocused a = a & focusedL .~ False & borderTypeL .~ Border.borderStyleFromChar ' '

