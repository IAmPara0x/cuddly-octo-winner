{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Lens (makeLenses, (^.), (%~), (.~))
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

import Miku.Draw             (W, Draw(..), draw, defDraw, focusedL, borderTypeL)
import Miku.Draw.CurrentTask (CurrentTask(NoCurrentTask, CurrentTask))
import Miku.Draw.Goals       (CompletedGoals(..), NotCompletedGoals(..))
import Miku.Draw.StatusLine  (StatusLine(..))
import Miku.Mode
  ( Action
  , handleAnyStateEvent
  , haltAction
  , continueAction
  , gsModeStateL
  , gsEditingModeL
  , IsMode(..)
  , KeyMap
  , Name
  , DrawMode
  )

import Miku.Types.Window

import System.FilePath ((</>))

import Relude hiding (Either(..))


-- TODO: remove this
type Stats = Widget Name

type AllWindows = '[ '( 'Top :# 'Left, CurrentTask)
                   , '( 'Top :# 'Right, Stats)
                   , '( 'Bottom :# 'Left, CompletedGoals)
                   , '( 'Bottom :# 'Right, NotCompletedGoals)
                   ]

data CurrentLog

data AnyCurrentLogState where
  AnyCurrentLogState :: Window (v :# h) -> CurrentLogState v h -> AnyCurrentLogState

data CurrentLogConfig =
  CurrentLogConfig { _clcConfigPathL    :: FilePath
                   , _clcClockAnimTimeL :: Int
                   }

data CurrentLogState (v :: VertPos) (h :: HorizPos) =
  CurrentLogState { _clsConfigL          :: CurrentLogConfig
                  , _clsCurrentWindowL   :: Window (v :# h)
                  , _clsLogL             :: Log
                  , _clsClockAnimStateL  :: Int
                  , _clsAllWindowsL      :: Windows AllWindows
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState


instance Default CurrentLogConfig where
  def = CurrentLogConfig
          { _clcConfigPathL = "/home/iamparadox/.miku/"
          , _clcClockAnimTimeL = 5
          }

instance IsMode CurrentLog where
  type ModeState CurrentLog = AnyCurrentLogState

  defState         = ( , currentLogStateActions) . AnyCurrentLogState (TopW :# LeftW)
                  <$> defCurrentLogState
  drawState        = drawCurrentLogState
  handleEventState = handleAnyStateEvent


defCurrentLogState :: IO (CurrentLogState 'Top 'Left)
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
              , _clsCurrentWindowL     = TopW :# LeftW
              , _clsClockAnimStateL    = 0
              , _clsAllWindowsL        = (TopW :# LeftW, currentTask)
                                      :> (TopW :# RightW, stats)
                                      :> (BottomW :# LeftW, completedGoals)
                                      :> (BottomW :# RightW, notCompletedGoals)
                                      :> WNil
              }

currentLogStateActions :: KeyMap CurrentLog
currentLogStateActions =
  Map.fromList [ ("q", exitApp)
               , ("k", up)
               , ("j", down)
               , ("l", right)
               , ("h", left)
               -- , ("<tab>", incAction)
               ]
  where

    exitApp :: Action CurrentLog
    exitApp  = haltAction

    up, down, left, right :: Action CurrentLog
    up    = modify (gsModeStateL %~ switchWindow TopW) >> continueAction
    down  = modify (gsModeStateL %~ switchWindow BottomW) >> continueAction
    left  = modify (gsModeStateL %~ switchWindow LeftW) >> continueAction
    right = modify (gsModeStateL %~ switchWindow RightW) >> continueAction


drawCurrentLogState :: DrawMode CurrentLog
drawCurrentLogState = do

  gstate <- ask
  (AnyCurrentLogState _ mstate) <- (^. gsModeStateL) <$> ask

  let

      -- TODO: Remove this
      allWindows = [ mapReader draw heading
                   , return $ draw $ window (TopW :# LeftW) (mstate ^. clsAllWindowsL)
                   , return $ draw $ window (TopW :# RightW) (mstate ^. clsAllWindowsL)
                   , return $ draw $ window (BottomW :# LeftW) (mstate ^. clsAllWindowsL)
                   , return $ draw $ window (BottomW :# RightW) (mstate ^. clsAllWindowsL)
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
  (AnyCurrentLogState _ mstate) <-  (^. gsModeStateL) <$> ask

  let logHeading = mstate ^. clsLogL . logHeadingL
      widget  = Core.padAll 1 $ Core.hCenter $ Core.txt $ showHeading logHeading

  return $ Draw { _focusedL    = False
                , _drawableL   = widget
                , _borderTypeL = Border.borderStyleFromChar ' '
                }

statusLine :: W CurrentLog StatusLine
statusLine = do

  gstate <- ask
  (AnyCurrentLogState _ mstate) <- (^. gsModeStateL) <$> ask

  let currWindow = case mstate ^. clsCurrentWindowL of
                         TopW :# LeftW     -> ["OngoingTask"]
                         TopW :# RightW    -> ["Stats"]
                         BottomW :# LeftW  -> ["TODO", "Completed"]
                         BottomW :# RightW -> ["TODO", "NotCompleted"]

      widget = StatusLine { _slEditingModeL = gstate ^. gsEditingModeL
                          , _slModeNameL = "CurrentLog"
                          , _slOtherInfoL = currWindow
                          }
  return $ Draw { _focusedL = False, _drawableL = widget, _borderTypeL = Border.unicode }

-- Helpers

type family SwitchWindow (a :: Layout) (b :: Layout) :: Layout where
  SwitchWindow ('Vert 'Top)    ('Stack _ b) = 'Top :# b
  SwitchWindow ('Vert 'Bottom) ('Stack _ b) = 'Bottom :# b
  SwitchWindow ('Horiz 'Left)  ('Stack b _) = b :# 'Left
  SwitchWindow ('Horiz 'Right) ('Stack b _) = b :# 'Right

type family SingleWindow (a :: Layout) :: Bool where
  SingleWindow ('Vert a)  = 'True
  SingleWindow ('Horiz a) = 'True
  SingleWindow _          = 'False

switchWindow :: (SingleWindow a ~ 'True) => Window a -> AnyCurrentLogState -> AnyCurrentLogState
switchWindow TopW (AnyCurrentLogState (_ :# h) mstate) = AnyCurrentLogState (TopW :# h)
                                                       $ mstate & clsCurrentWindowL %~ switch TopW
                                                                & changeFocus
switchWindow BottomW (AnyCurrentLogState (_ :# h) mstate) = AnyCurrentLogState (BottomW :# h)
                                                          $ mstate & clsCurrentWindowL %~ switch BottomW
                                                                   & changeFocus
switchWindow LeftW (AnyCurrentLogState (v :# _) mstate) = AnyCurrentLogState (v :# LeftW)
                                                        $ mstate & clsCurrentWindowL %~ switch LeftW
                                                                 & changeFocus
switchWindow RightW (AnyCurrentLogState (v :# _) mstate) = AnyCurrentLogState (v :# RightW)
                                                         $ mstate & clsCurrentWindowL %~ switch RightW
                                                                  & changeFocus

changeFocus :: CurrentLogState v h -> CurrentLogState v h
changeFocus s = s & clsAllWindowsL %~ mapWindows ((borderTypeL .~ Border.borderStyleFromChar ' ') . (focusedL .~ False))
                  & modifyCurrWindow ((borderTypeL .~ Border.unicodeRounded) . (focusedL .~ True))


switch :: forall a b c. (SingleWindow a ~ 'True)
       => Window a -> Window (b :# c) -> Window (SwitchWindow a (b :# c))
switch TopW (_ :# b)    = TopW :# b
switch BottomW (_ :# b) = BottomW :# b
switch LeftW (b :# _)   = b :# LeftW
switch RightW (b :# _)  = b :# RightW

currWindowState :: CurrentLogState v h -> Draw (Lookup (v :# h) AllWindows)
currWindowState CurrentLogState{_clsCurrentWindowL, _clsAllWindowsL}
  = case _clsCurrentWindowL of
      (TopW :# LeftW)     -> window (TopW :# LeftW) _clsAllWindowsL
      (TopW :# RightW)    -> window (TopW :# RightW) _clsAllWindowsL
      (BottomW :# LeftW)  -> window (BottomW :# LeftW) _clsAllWindowsL
      (BottomW :# RightW) -> window (BottomW :# RightW) _clsAllWindowsL

modifyCurrWindow :: (Draw (Lookup (v :# h) AllWindows) -> Draw (Lookup (v :# h) AllWindows))
                 -> CurrentLogState v h
                 -> CurrentLogState v h
modifyCurrWindow f mstate@CurrentLogState{_clsCurrentWindowL, _clsAllWindowsL}
  = case _clsCurrentWindowL of
      (TopW :# LeftW)     -> mstate {_clsAllWindowsL = modifyW (TopW :# LeftW) f _clsAllWindowsL}
      (TopW :# RightW)    -> mstate {_clsAllWindowsL = modifyW (TopW :# RightW) f _clsAllWindowsL}
      (BottomW :# LeftW)  -> mstate {_clsAllWindowsL = modifyW (BottomW :# LeftW) f _clsAllWindowsL}
      (BottomW :# RightW) -> mstate {_clsAllWindowsL = modifyW (BottomW :# RightW) f _clsAllWindowsL}
