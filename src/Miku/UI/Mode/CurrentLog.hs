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

import Control.Lens (makeLenses, (^.), (.~), (%~), _1, _2)
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

import Miku.UI.Draw (Border(..), Drawable(..))
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
                   }

data CurrentLogState =
  CurrentLogState { _clsConfigL        :: CurrentLogConfig
                  , _clsCurrentWindowL :: Window
                  , _clsKeyMapL        :: KeyMap CurrentLogState
                  , _clsLogL           :: Log
                  , _clsPrevKeysL      :: Keys
                  , _clsTickCounterL   :: Integer
                  }

makeLenses ''CurrentLogConfig
makeLenses ''CurrentLogState

instance Default CurrentLogConfig where
  def = CurrentLogConfig
          { _clcConfigPathL = "/home/iamparadox/.miku/"
          , _clcClearKeysTimeL = 5
          , _clcMaxTicksCountL = 100
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
                          { _clsConfigL   = def
                          , _clsKeyMapL   = currentLogStateActions
                          , _clsLogL      = log
                          , _clsPrevKeysL = []
                          , _clsCurrentWindowL = (TopWindow, LeftWindow)
                          , _clsTickCounterL = 0
                          }

handleCurrentLogEvent :: CurrentLogState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleCurrentLogEvent clState (AppEvent Tick)
    = Brick.continue
    $ AppState
    $ clState & clsTickCounterL %~ (\n -> mod (n + 1) (clState ^. clsConfigL . clcMaxTicksCountL))
              & id %~ clearPrevKeys
handleCurrentLogEvent clState event          
    = handleAnyStateEvent (clState & clsTickCounterL .~ 0) event

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

    down :: Action CurrentLogState
    down clState = Brick.continue $ AppState (clState & clsCurrentWindowL . _1 .~ BottomWindow)

    up :: Action CurrentLogState
    up clState = Brick.continue $ AppState (clState & clsCurrentWindowL . _1 .~ TopWindow)

    left :: Action CurrentLogState
    left clState = Brick.continue $ AppState (clState & clsCurrentWindowL . _2 .~ LeftWindow)

    right :: Action CurrentLogState
    right clState = Brick.continue $ AppState (clState & clsCurrentWindowL . _2 .~ RightWindow)


clearPrevKeys :: CurrentLogState -> CurrentLogState
clearPrevKeys clState
  = clState & clsPrevKeysL %~
      (\keys -> if rem (clState ^. clsTickCounterL) (clState ^. clsConfigL . clcClearKeysTimeL) == 0 then [] else keys)

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
drawCurrentTaskWindow clState
  | clState ^. clsCurrentWindowL == (TopWindow, LeftWindow)
      = draw Rounded
      $ maybe (NoCurrentTask "There' currently no ongoing task.") CurrentTask (ongoingTask $ clState ^. clsLogL)
  | otherwise
      = draw Hidden
      $ maybe (NoCurrentTask "There' currently no ongoing task.") CurrentTask (ongoingTask $ clState ^. clsLogL)

drawStatsWindow :: CurrentLogState -> Widget n
drawStatsWindow clState
  | clState ^. clsCurrentWindowL == (TopWindow, RightWindow)
      = Core.withBorderStyle Border.unicodeRounded $ Border.border
      $ Core.center $ Core.txt "No Stats!"
  | otherwise
      = Core.center $ Core.txt "No Stats!"

drawNotCompletedGoalsWindow :: CurrentLogState -> Widget n
drawNotCompletedGoalsWindow clState
  | clState ^. clsCurrentWindowL == (BottomWindow, RightWindow)
      = Core.padLeft (Pad 1)
      $ draw Rounded
      $ coerce @_ @NotCompletedGoals
      $ goalsNotDone (clState ^. clsLogL . logGoalsL)
  | otherwise
      = Core.padLeft (Pad 1)
      $ draw Hidden
      $ coerce @_ @NotCompletedGoals
      $ goalsNotDone (clState ^. clsLogL . logGoalsL)

drawCompletedGoalsWindow :: CurrentLogState -> Widget n
drawCompletedGoalsWindow clState
  | clState ^. clsCurrentWindowL == (BottomWindow, LeftWindow)
      = Core.padLeft (Pad 1)
      $ draw Rounded
      $ coerce @_ @CompletedGoals
      $ goalsDone (clState ^. clsLogL . logGoalsL)
  | otherwise
      = Core.padLeft (Pad 1)
      $ draw Hidden
      $ coerce @_ @CompletedGoals
      $ goalsDone (clState ^. clsLogL . logGoalsL)


toCurrentLogMode :: forall a. IsMode a => Action a
toCurrentLogMode _ = liftIO (defState @CurrentLogState) >>= Brick.continue . AppState
