{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
                                   draw, drawableL, focusedL)
import Miku.Draw.CurrentTask      (CurrentTask (CurrentTask, NoCurrentTask))
import Miku.Draw.StatusLine       (StatusInfo (..), StatusLine (..),
                                   StatusLineInfo (..))
import Miku.Draw.Todos            (Completed, NotCompleted, Todos,
                                   changeTodoIdx, mkCompletedTodos,
                                   mkNotCompletedTodos)
import Miku.Mode                  (Action, DrawMode, GlobalState, IsMode (..),
                                   KeyMap (..), Name, continueAction,
                                   gsEditingModeL, gsModeStateL, haltAction,
                                   handleAnyStateEvent, toNormalMode)

import Miku.Editing               (EditingMode (..))
import Miku.Types.Window          (HorizPos (..), KnownWindow (wGet, wModify),
                                   Layout (Horiz, Vert), Lookup, VertPos (..),
                                   Window (..), Windows (..), type (:#), wMap,
                                   wToList)

import System.FilePath            ((</>))

import Relude                     hiding (Either (..))


-- TODO: remove this
newtype Stats
  = Stats (Widget Name)
  deriving newtype (Drawable)

instance StatusLineInfo Stats where
  statusLineInfo _ = ["Stats"]

type AllWindows = '[ '( 'Top :# 'Left, CurrentTask)
                   , '( 'Top :# 'Right, Stats)
                   , '( 'Bottom :# 'Left, Todos NotCompleted)
                   , '( 'Bottom :# 'Right, Todos Completed)
                   ]

class (Drawable a, StatusLineInfo a) => WindowConstraints a
instance (Drawable a, StatusLineInfo a) => WindowConstraints a

type ModifyWindow v h = Draw (Lookup (v :# h) AllWindows) -> Draw (Lookup (v :# h) AllWindows)

data CurrentLog
  = CurrentLog
  deriving stock (Show)

data AnyCurrentLogState where
  AnyCurrentLogState :: CurrentLogState v h -> AnyCurrentLogState

data CurrentLogConfig
  = CurrentLogConfig
      { _clcConfigPathL    :: FilePath
      , _clcClockAnimTimeL :: Int
      }

data CurrentLogState (v :: VertPos) (h :: HorizPos)
  = CurrentLogState
      { _clsConfigL         :: CurrentLogConfig
      , _clsCurrentWindowL  :: Window (v :# h)
      , _clsLogL            :: Log
      , _clsClockAnimStateL :: Int
      , _clsAllWindowsL     :: Windows WindowConstraints AllWindows
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
  type ModeState CurrentLog = AnyCurrentLogState

  defState         = ( , currentLogStateActions) . switchWindow WTop . AnyCurrentLogState
                  <$> defCurrentLogState
  drawState        = drawCurrentLogState
  handleEventState = handleAnyStateEvent


defCurrentLogState :: IO (CurrentLogState 'Top 'Left)
defCurrentLogState =
  do

    elog <- runExceptT $ readCurrentLog (def ^. clcConfigPathL </> "logs")

    let log = either error id elog

        -- completedTodos :: _
        completedTodos    = defDraw $ mkCompletedTodos 0 $ todosDone $ log  ^. logTodosL

        notCompletedTodos = defDraw $ mkNotCompletedTodos 0 $ todosNotDone $ log  ^. logTodosL

        currentTask       = defDraw $ maybe (NoCurrentTask "There's currently no ongoing task.")
                                            (CurrentTask 0) $ ongoingTask log

        stats             = defDraw $ Stats $ Border.border $ Core.center $ Core.txt "No Stats!"

    return $ CurrentLogState
              { _clsConfigL            = def
              , _clsLogL               = log
              , _clsCurrentWindowL     = WTop :# WLeft
              , _clsClockAnimStateL    = 0
              , _clsAllWindowsL        = (WTop :# WLeft, currentTask)
                                      :> (WTop :# WRight, stats)
                                      :> (WBottom :# WLeft, notCompletedTodos)
                                      :> (WBottom :# WRight, completedTodos)
                                      :> WNil
              }

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions = KeyMap { _normalModeMapL = normalKeyMap
                                , _insertModeMapL = Map.fromList [("jk", toNormalMode)]
                                }

  where

    normalKeyMap =
      Map.fromList [ ("q", exitApp)
                   , ("k", up)
                   , ("j", down)
                   , ("l", right)
                   , ("h", left)
                   , ("<tab>", incAction)
                   , ("<shift>+<tab>", decAction)
                   ]

    exitApp :: Action 'Normal CurrentLog
    exitApp  = haltAction

    up, down, left, right :: Action 'Normal CurrentLog
    up    = modify (gsModeStateL %~ switchWindow WTop) >> continueAction
    down  = modify (gsModeStateL %~ switchWindow WBottom) >> continueAction
    left  = modify (gsModeStateL %~ switchWindow WLeft) >> continueAction
    right = modify (gsModeStateL %~ switchWindow WRight) >> continueAction

    incAction = modify (gsModeStateL %~ modifyAnyCurrentLogState (changeFocus 1))
              >> continueAction
    decAction = modify (gsModeStateL %~ modifyAnyCurrentLogState (changeFocus (-1)))
              >> continueAction

    changeFocus :: Int -> CurrentLogState v h -> CurrentLogState v h
    changeFocus n = modifyAllWindow (id , id, drawableL %~ changeTodoIdx n, drawableL %~ changeTodoIdx n)


drawCurrentLogState :: DrawMode emode CurrentLog
drawCurrentLogState = do

  gstate <- ask
  (AnyCurrentLogState mstate) <- (^. gsModeStateL) <$> ask

  h <- mapReader draw heading
  let allWindows :: [Widget Name]
      allWindows = h : wToList draw (mstate ^. clsAllWindowsL)

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
  (AnyCurrentLogState mstate) <-  (^. gsModeStateL) <$> ask

  let logHeading = mstate ^. clsLogL . logHeadingL
      widget  = Core.padAll 1 $ Core.hCenter $ Core.txt $ showHeading logHeading

  return $ Draw { _focusedL    = False
                , _drawableL   = widget
                , _borderTypeL = Border.borderStyleFromChar ' '
                }

statusLine :: W emode CurrentLog (StatusLine emode)
statusLine = do

  (gstate :: GlobalState emode CurrentLog) <- ask
  (AnyCurrentLogState (mstate :: CurrentLogState v h)) <- (^. gsModeStateL) <$> ask

  let widget :: StatusLine emode
      widget = StatusLine { _slEditingModeL = gstate ^. gsEditingModeL
                          , _slInfoL = [ StatusInfo CurrentLog
                                       , windowStatusInfo
                                       ]
                          }

      windowStatusInfo = case mstate ^. clsCurrentWindowL of
                            WTop :# WLeft  -> StatusInfo $ currWindowState mstate
                            WTop :# WRight -> StatusInfo $ currWindowState mstate
                            WBottom :# WLeft -> StatusInfo $ currWindowState mstate
                            WBottom :# WRight -> StatusInfo $ currWindowState mstate

  return $ Draw { _focusedL = False, _drawableL = widget, _borderTypeL = Border.unicode }

-- Helpers
type family SingleWindow (a :: Layout) :: Bool where
  SingleWindow ('Vert a)  = 'True
  SingleWindow ('Horiz a) = 'True
  SingleWindow _          = 'False

switchWindow :: (SingleWindow a ~ 'True) => Window a -> AnyCurrentLogState -> AnyCurrentLogState
switchWindow newPos (AnyCurrentLogState mstate)
  = case newPos of
      WTop    -> AnyCurrentLogState
              $ mstate & clsCurrentWindowL .~ (WTop :# h)
                       & changeFocus
      WBottom -> AnyCurrentLogState
              $ mstate & clsCurrentWindowL .~ (WBottom :# h)
                       & changeFocus
      WLeft   -> AnyCurrentLogState
              $ mstate & clsCurrentWindowL .~ (v :# WLeft)
                       & changeFocus
      WRight  -> AnyCurrentLogState
              $ mstate & clsCurrentWindowL .~ (v :# WRight)
                       & changeFocus
  where

    changeFocus :: CurrentLogState v h -> CurrentLogState v h
    changeFocus s = let news = s & clsAllWindowsL %~ wMap offFocus in
      case s ^. clsCurrentWindowL of
          WTop :# WLeft     -> modifyCurrWindow onFocus news
          WTop :# WRight    -> modifyCurrWindow onFocus news
          WBottom :# WLeft  -> modifyCurrWindow onFocus news
          WBottom :# WRight -> modifyCurrWindow onFocus news

    onFocus, offFocus :: Draw a -> Draw a
    onFocus d = d & borderTypeL .~ Border.unicodeRounded
                  & focusedL .~ True
    offFocus d = d & borderTypeL .~ Border.borderStyleFromChar ' '
                   & focusedL .~ False

    (v :# h) = mstate ^. clsCurrentWindowL


currWindowState :: KnownWindow (v :# h) AllWindows => CurrentLogState v h -> Lookup (v :# h) AllWindows
currWindowState CurrentLogState{_clsCurrentWindowL, _clsAllWindowsL}
  = wGet _clsCurrentWindowL _clsAllWindowsL ^. drawableL

-- TODO: Using something better instead of Tuple.
modifyAllWindow :: ( ModifyWindow 'Top 'Left
                   , ModifyWindow 'Top 'Right
                   , ModifyWindow 'Bottom 'Left
                   , ModifyWindow 'Bottom 'Right
                   )
                -> CurrentLogState v h
                -> CurrentLogState v h
modifyAllWindow (tl, tr, bl, br)
  mstate@CurrentLogState{_clsCurrentWindowL, _clsAllWindowsL}
  = case _clsCurrentWindowL of
      (WTop :# WLeft)     -> mstate {_clsAllWindowsL = wModify (WTop :# WLeft) tl _clsAllWindowsL}
      (WTop :# WRight)    -> mstate {_clsAllWindowsL = wModify (WTop :# WRight) tr _clsAllWindowsL}
      (WBottom :# WLeft)  -> mstate {_clsAllWindowsL = wModify (WBottom :# WLeft) bl _clsAllWindowsL}
      (WBottom :# WRight) -> mstate {_clsAllWindowsL = wModify (WBottom :# WRight) br _clsAllWindowsL}

modifyCurrWindow :: KnownWindow (v :# h) AllWindows
                 => ModifyWindow v h
                 -> CurrentLogState v h
                 -> CurrentLogState v h
modifyCurrWindow f
  mstate@CurrentLogState{_clsCurrentWindowL, _clsAllWindowsL}
    = mstate {_clsAllWindowsL = wModify _clsCurrentWindowL f _clsAllWindowsL}

modifyAnyCurrentLogState :: (forall v h. CurrentLogState v h -> CurrentLogState v h)
                         -> AnyCurrentLogState
                         -> AnyCurrentLogState
modifyAnyCurrentLogState f (AnyCurrentLogState mstate) = AnyCurrentLogState $ f mstate
