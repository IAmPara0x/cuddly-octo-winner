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
import Data.Default               (Default (def))
import Data.Map                   qualified as Map

import Miku.Templates.Log         (Log)
import Miku.Templates.Log         qualified as Log

import Miku.Draw                  (Draw (..), Drawable, defDraw, draw)
import Miku.Draw                  qualified as Draw
import Miku.Draw.CurrentTask
  ( CurrentTask (CurrentTask, NoCurrentTask)
  , CurrentTaskItem (TaskName)
  , changeCurrentTaskFocus
  )
import Miku.Draw.StatusLine       (StatusInfo (StatusInfo), StatusLineInfo (..))
import Miku.Draw.StatusLine       qualified as StatusLine
import Miku.Draw.Todos            (Completed, NotCompleted, Todos (..))
import Miku.Draw.Todos            qualified as Todos
import Miku.Events                qualified as Events
import Miku.Mode
  ( Action
  , DrawMode
  , GlobalState
  , IsMode (..)
  , KeyMap (..)
  , Keys
  , gsModeStateL
  , gsStatusLineL
  )

import Miku.Editing               (EditingMode (..))
import Miku.Mode.Utility          (Window)
import Miku.Mode.Utility          qualified as Utils

import Miku.Resource              (Res)

import Miku.Types.Rec             (Rec (..), rmap, rmodify)
import Miku.Types.Rec             qualified as Rec
import System.FilePath            ((</>))

import Relude

-- TODO: Remove all errors and replace them with EitherT or Freer Monad

-- TODO: remove this
newtype Stats
  = Stats (Widget Res)
  deriving newtype (Drawable Draw)

instance StatusLineInfo Stats where
  statusLineInfo _ = ["Stats"]


data CurrentLog
  = CurrentLog
  deriving stock (Show)

newtype CurrentLogConfig
  = CurrentLogConfig { _configPathL :: FilePath }

data CurrentLogState
  = CurrentLogState
      { _configL     :: CurrentLogConfig
      , _windowL     :: Window 2 2
      , _logL        :: Log
      , _allWindowsL :: Rec '[Drawable Draw, StatusLineInfo] WindowStates Draw
      }

type WindowStates = '[CurrentTask , Stats , Todos NotCompleted , Todos Completed]

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState


instance Default CurrentLogConfig where
  def = CurrentLogConfig { _configPathL = "/home/iamparadox/.miku/" }

instance StatusLineInfo CurrentLog where
  statusLineInfo x = [show x]

instance IsMode CurrentLog where
  type ModeState CurrentLog = CurrentLogState

  defState = (, currentLogStateActions) . switchWindow (Utils.vertMove 1) <$> defCurrentLogState
  drawState = drawCurrentLogState
  handleEventState = Events.handleAnyStateEvent


defCurrentLogState :: IO CurrentLogState
defCurrentLogState = do

  elog <- runExceptT $ Log.readCurrentLog (def ^. configPathL </> "logs")

  let
    log            = either error id elog

    completedTodos = defDraw $ Todos.mkCompletedTodos 0 $ Log.todosDone $ log ^. Log.logTodosL

    notCompletedTodos =
      defDraw $ Todos.mkNotCompletedTodos 0 $ Log.todosNotDone $ log ^. Log.logTodosL

    currentTask =
      defDraw
        $ maybe (NoCurrentTask "There's currently no ongoing task.") (CurrentTask TaskName)
        $ Log.ongoingTask log

    stats = defDraw $ Stats $ Border.border $ Core.center $ Core.txt "No Stats!"

  return $ CurrentLogState
    { _configL     = def
    , _windowL     = Utils.window 0 0
    , _logL        = log
    , _allWindowsL = currentTask :> stats :> notCompletedTodos :> completedTodos :> RNil
    }

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions = KeyMap { _normalModeMapL = normalKeyMap
                                , _insertModeMapL = Map.fromList [("jk", Events.toNormalMode)]
                                }

 where

  normalKeyMap :: Map Keys (Action 'Normal CurrentLog)
  normalKeyMap = Map.fromList
    [ ("q"            , Events.halt)
    , ("k"            , up)
    , ("j"            , down)
    , ("l"            , right)
    , ("h"            , left)
    , ("<tab>"        , incAction)
    , ("<shift>+<tab>", decAction)
    ]

  right, left, up, down :: Action 'Normal CurrentLog
  right = modify (gsModeStateL %~ switchWindow (Utils.horizMove 1))
    >> Events.modifyAndContinue updateStatusLine
  left = modify (gsModeStateL %~ switchWindow (Utils.horizMove (-1)))
    >> Events.modifyAndContinue updateStatusLine
  up = modify (gsModeStateL %~ switchWindow (Utils.vertMove 1))
    >> Events.modifyAndContinue updateStatusLine
  down = modify (gsModeStateL %~ switchWindow (Utils.vertMove (-1)))
    >> Events.modifyAndContinue updateStatusLine

  incAction, decAction :: Action 'Normal CurrentLog
  incAction =
    modify (gsModeStateL . allWindowsL %~ changeIdx 1) >> Events.modifyAndContinue updateStatusLine
  decAction = modify (gsModeStateL . allWindowsL %~ changeIdx (-1))
    >> Events.modifyAndContinue updateStatusLine

  changeIdx :: Int -> Rec cs WindowStates Draw -> Rec cs WindowStates Draw
  changeIdx n =
    Rec.rfmap _focusedL
      $  Identity (changeCurrentTaskFocus n)
      :> Identity id
      :> Identity (Todos.changeTodoIdx n)
      :> Identity (Todos.changeTodoIdx n)
      :> RNil

  updateStatusLine :: GlobalState 'Normal CurrentLog -> GlobalState 'Normal CurrentLog
  updateStatusLine gstate =
    gstate & gsStatusLineL . StatusLine.slInfoL .~ [StatusInfo CurrentLog, windowStatusInfo]
   where
    allWindows = gstate ^. gsModeStateL . allWindowsL

    windowStatusInfo :: StatusInfo
    windowStatusInfo = case Rec.rfilterMap _focusedL (StatusInfo . _drawableL) allWindows of
      [x] -> x
      _   -> error "There must be atleast one window focued"


drawCurrentLogState :: DrawMode emode CurrentLog
drawCurrentLogState = do

  CurrentLogState {..} <- (^. gsModeStateL) <$> ask

  let
    logHeading =
      Core.padAll 1 $ Core.hCenter $ Core.txt $ Log.showHeading (_logL ^. Log.logHeadingL)

    allWindows :: [Widget Res]
    allWindows = logHeading : Rec.toList draw _allWindowsL

    drawAllWindows :: [Widget Res] -> Widget Res
    drawAllWindows [headingWindow, topWLeftindow, topWRightindow, botWLeftindow, botWRightindow] =
      Core.vBox
        [ headingWindow
        , Core.hBox [topWLeftindow, topWRightindow]
        , Core.padTop (Pad 1) $ Core.hBox [botWLeftindow, botWRightindow]
        ]
    drawAllWindows _ = error "x_x"

  return $ drawAllWindows allWindows

switchWindow :: (Window 2 2 -> Window 2 2) -> CurrentLogState -> CurrentLogState
switchWindow f x = x & windowL %~ f & changeFocus
 where

  changeFocus :: CurrentLogState -> CurrentLogState
  changeFocus mstate = case Utils.viewWindow (mstate ^. windowL) of
    (0, 0) -> mstate & allWindowsL %~ rmodify @(Todos NotCompleted) focused . rmap notfocused
    (1, 0) -> mstate & allWindowsL %~ rmodify @(Todos Completed) focused . rmap notfocused
    (0, 1) -> mstate & allWindowsL %~ rmodify @CurrentTask focused . rmap notfocused
    (1, 1) -> mstate & allWindowsL %~ rmodify @Stats focused . rmap notfocused
    a      -> error $ "Window of coord " <> show a <> " is not possible!"

  focused :: Draw a -> Draw a
  focused a = a & Draw.focusedL .~ True & Draw.borderTypeL .~ Border.unicodeRounded

  notfocused :: Draw a -> Draw a
  notfocused a = a & Draw.focusedL .~ False & Draw.borderTypeL .~ Border.borderStyleFromChar ' '
