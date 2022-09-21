module Miku.Mode.CurrentLog
  ( CurrentLog
  , currentLogStateActions
  , initCurrentLogMode
  , toCurrentLogMode
  ) where

import Brick.Main            qualified as Brick
import Brick.Widgets.Border  qualified as Border
import Brick.Widgets.Center  qualified as Core
import Brick.Widgets.Core    qualified as Core

import Brick.Types           (Padding (Pad), Widget)

import Control.Lens          (makeLenses, (%~), (.~), (^.))
import Data.Default          (Default (def))
import Data.Map              qualified as Map

import Miku.Templates.Log    (Log)
import Miku.Templates.Log    qualified as Log

import Miku.Draw             (AnyWidget (..), InitWidget (..), W (..))
import Miku.Draw             qualified as Draw
import Miku.Draw.CurrentTask
  ( CurrentTask (CurrentTask, NoCurrentTask)
  , CurrentTaskItem (TaskName)
  , Stats (Stats)
  )
import Miku.Draw.StatusLine  qualified as StatusLine
import Miku.Draw.Todos       (Completed, NotCompleted, Todos (..))
import Miku.Draw.Todos       qualified as Todos
import Miku.Events           qualified as Events
import Miku.Mode
  ( Action
  , ActionM
  , AppState (AppState)
  , DrawMode
  , GlobalState
  , IsMode (..)
  , KeyMap (..)
  , Keys
  , gsModeStateL
  , gsStatusLineL
  )
import Miku.Mode             qualified as Mode

import Miku.Editing          (EditingMode (..))
import Miku.Mode.Utility     (FocusRing2D)
import Miku.Mode.Utility     qualified as Utils

import Miku.Resource         (Res)

import System.FilePath       ((</>))

import Control.Monad.Except  (mapExceptT)
import Relude

data CurrentLog
  = CurrentLog
  deriving stock (Show)

newtype CurrentLogConfig
  = CurrentLogConfig { _logDirL :: FilePath }

data CurrentLogState
  = CurrentLogState
      { _configL  :: CurrentLogConfig
      , _windowsL :: FocusRing2D 2 2 AnyWidget
      , _logL     :: Log
      }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState

instance Default CurrentLogConfig where
  def = CurrentLogConfig { _logDirL = Mode._gcPathL def </> "logs" }

instance IsMode CurrentLog where
  type ModeState CurrentLog = CurrentLogState
  type ModeConf CurrentLog = CurrentLogConfig

  drawstate        = drawCurrentLogState
  handleEventState = Events.handleAnyStateEvent

toCurrentLogMode :: forall a . Action 'Normal a
toCurrentLogMode = do
  mstate <- mapExceptT liftIO defCurrentLogState
  mconf  <- liftIO def
  gstate <- get

  Mode.liftEvent
    $ Brick.continue (AppState $ gstate & Mode.gsModeL .~ (mstate, mconf, currentLogStateActions))

initCurrentLogMode :: ExceptT Text IO (GlobalState 'Normal CurrentLog)
initCurrentLogMode = do
  mstate <- defCurrentLogState
  return $ Mode.initGlobalState mstate def currentLogStateActions

defCurrentLogState :: ExceptT Text IO CurrentLogState
defCurrentLogState = do

  log <- Log.readCurrentLog (_logDirL def)

  let completedTodos :: W (Todos Completed)
      completedTodos = initWidget $ Todos.mkCompletedTodos 0 $ Log.todosDone $ log ^. Log.logTodosL

      notCompletedTodos :: W (Todos NotCompleted)
      notCompletedTodos =
        initWidget $ Todos.mkNotCompletedTodos 0 $ Log.todosNotDone $ log ^. Log.logTodosL

      currentTask :: W CurrentTask
      currentTask =
        initWidget
          $ maybe (NoCurrentTask "There's currently no ongoing task.") (CurrentTask TaskName)
          $ Log.ongoingTask log

      stats :: W Stats
      stats = initWidget $ Stats $ Border.border $ Core.center $ Core.txt "No Stats!"

      allwindows :: Map (Int, Int) AnyWidget
      allwindows = fromList
        [ ((0, 0), AnyWidget notCompletedTodos)
        , ((1, 0), AnyWidget completedTodos)
        , ((0, 1), AnyWidget currentTask)
        , ((1, 1), AnyWidget stats)
        ]

  windows <- hoistEither $ Utils.focusRing2D (0, 0) allwindows

  return $ CurrentLogState { _configL = def, _windowsL = windows, _logL = log }

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
  right = switch (Utils.horizMove 1)
  left  = switch (Utils.horizMove (-1))
  up    = switch (Utils.vertMove 1)
  down  = switch (Utils.vertMove (-1))

  incAction :: Action 'Normal CurrentLog
  incAction =
    Events.modifyAndContinue
        (gsModeStateL . windowsL %~ Utils.modifyFocused (Draw.changeAnyWidgetFocus 1))
      >> updateStatusLine
      >> Events.continue

  decAction :: Action 'Normal CurrentLog
  decAction =
    Events.modifyAndContinue
        (gsModeStateL . windowsL %~ Utils.modifyFocused (Draw.changeAnyWidgetFocus (-1)))
      >> updateStatusLine
      >> Events.continue

  switch :: (FocusRing2D 2 2 AnyWidget -> FocusRing2D 2 2 AnyWidget) -> Action 'Normal CurrentLog
  switch f = switchWindow f >> Events.continue

drawCurrentLogState :: DrawMode emode CurrentLog
drawCurrentLogState = do

  CurrentLogState {..} <- (^. gsModeStateL) <$> ask

  let
    logHeading :: Widget Res
    logHeading =
      Core.padAll 1 $ Core.hCenter $ Core.txt $ Log.showHeading (_logL ^. Log.logHeadingL)

    x :: FocusRing2D 2 2 (Widget Res)
    x = fmap Draw.drawAnyWidget _windowsL

    drawAllWindows :: [Widget Res] -> Either Text (Widget Res)
    drawAllWindows [headingWindow, botWLeftindow, botWRightindow, topWLeftindow, topWRightindow] =
      Right $ Core.vBox
        [ headingWindow
        , Core.hBox [topWLeftindow, topWRightindow]
        , Core.padTop (Pad 1) $ Core.hBox [botWLeftindow, botWRightindow]
        ]
    drawAllWindows _ = Left "Expected only 5 elements in the list."

  allwindows <- hoistEither $ mapM (`Utils.getAny` x) [(0, 0), (1, 0), (0, 1), (1, 1)]

  hoistEither $ drawAllWindows (logHeading : allwindows)


switchWindow
  :: (FocusRing2D 2 2 AnyWidget -> FocusRing2D 2 2 AnyWidget) -> ActionM 'Normal CurrentLog ()
switchWindow switch = do
  modify (gsModeStateL . windowsL %~ Utils.modifyFocused Draw.unfocusAnyWidget)
  modify (gsModeStateL . windowsL %~ Utils.modifyFocused Draw.focusAnyWidget . switch)
  updateStatusLine

updateStatusLine :: ActionM 'Normal CurrentLog ()
updateStatusLine = do
  gstate        <- get
  (AnyWidget w) <- hoistEither (Utils.getFocused $ gstate ^. gsModeStateL . windowsL)
  let windowStatusInfo = Draw._statusLineInfoL w w
  put (gstate & gsStatusLineL . StatusLine.slInfoL .~ ["CurrentLog", windowStatusInfo])
