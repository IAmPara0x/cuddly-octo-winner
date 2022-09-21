{-# LANGUAGE NamedFieldPuns #-}
module Miku.Draw.Todos
  ( Completed
  , NotCompleted
  , Todos (_currTodoIdxL, _getTodosL)
  , currTodoIdxL
  , getTodosL
  , mkCompletedTodos
  , mkNotCompletedTodos
  ) where

import Brick.Types                (Padding (Pad), Widget)

import Brick.Widgets.Border       qualified as Border
import Brick.Widgets.Border.Style qualified as Border
import Brick.Widgets.Center       qualified as Core
import Brick.Widgets.Core         qualified as Core

import Control.Lens               (ifolded, makeLenses, to, withIndex, (%~), (^.), (^..))

import Miku.Templates.Log         (Todo)
import Miku.Templates.Log         qualified as Log

import Miku.Draw                  (InitWidget (..), W (..))
import Miku.Draw                  qualified as Draw
import Miku.Draw.StatusLine       (StatusInfo (StatusInfo))
import Miku.Resource              (Res)

import Relude

data Completed
data NotCompleted
data Todos a
  = Todos
      { _currTodoIdxL :: Int
      , _getTodosL    :: [Todo]
      }
  | NoTodos

deriving stock instance Eq (Todos a)

makeLenses ''Todos

mkCompletedTodos :: Int -> [Todo] -> Todos Completed
mkCompletedTodos _ []    = NoTodos
mkCompletedTodos n todos = Todos (mod n $ length todos) todos

mkNotCompletedTodos :: Int -> [Todo] -> Todos NotCompleted
mkNotCompletedTodos _ []    = NoTodos
mkNotCompletedTodos n todos = Todos (mod n $ length todos) todos

instance InitWidget (Todos Completed) where
  initWidget a = W { _focusedL        = False
                   , _borderTypeL     = Border.borderStyleFromChar ' '
                   , _widgetStateL    = a
                   , _drawL           = drawTodos "[✓] Completed"
                   , _statusLineInfoL = completedTodosInfo . _widgetStateL
                   , _changeFocusL    = \n -> Draw.widgetStateL %~ changeTodoIdx n
                   }

instance InitWidget (Todos NotCompleted) where
  initWidget a = W { _focusedL        = False
                   , _borderTypeL     = Border.borderStyleFromChar ' '
                   , _widgetStateL    = a
                   , _drawL           = drawTodos "[✕] Not Completed"
                   , _statusLineInfoL = notCompletedTodosInfo . _widgetStateL
                   , _changeFocusL    = \n -> Draw.widgetStateL %~ changeTodoIdx n
                   }

changeTodoIdx :: Int -> Todos a -> Todos a
changeTodoIdx _ NoTodos           = NoTodos
changeTodoIdx n (Todos idx todos) = Todos (mod (n + idx) $ length todos) todos

completedTodosInfo :: Todos Completed -> StatusInfo
completedTodosInfo Todos {..} =
  StatusInfo ["Todos", "Completed", "[" <> show (_currTodoIdxL + 1) <> "]"]
completedTodosInfo NoTodos = StatusInfo ["Todos", "Completed"]

notCompletedTodosInfo :: Todos NotCompleted -> StatusInfo
notCompletedTodosInfo Todos {..} =
  StatusInfo ["Todos", "NotCompleted", "[" <> show (_currTodoIdxL + 1) <> "]"]
notCompletedTodosInfo NoTodos = StatusInfo ["Todos", "NotCompleted"]

drawTodos :: Text -> W (Todos a) -> Widget Res
drawTodos heading W {..} = Core.withBorderStyle _borderTypeL $ case _widgetStateL of
  Todos currIdx todos -> Border.border $ Core.center $ Core.vBox
    [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
    , Core.vBox (todos ^.. ifolded . withIndex . to (addAttr currIdx))
    , Core.fill ' '
    ]
  NoTodos -> Border.border $ Core.center $ Core.vBox
    [Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading, Core.fill ' ']

 where
  addAttr currIdx (idx, todo) | not _focusedL  = Core.withAttr "todo" $ drawTodo todo
                              | idx == currIdx = Core.withAttr "current" $ drawTodo todo
                              | otherwise      = drawTodo todo

drawTodo :: Todo -> Widget Res
drawTodo todo =
  Core.padBottom (Pad 1) $ Core.hBox [Core.padLeft (Pad 1) $ Core.txt $ todo ^. Log.todoDescL]
