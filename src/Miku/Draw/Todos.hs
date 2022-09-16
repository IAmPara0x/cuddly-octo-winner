{-# LANGUAGE NamedFieldPuns #-}
module Miku.Draw.Todos
  ( Completed
  , NotCompleted
  , Todos (_currTodoIdxL, _getTodosL)
  , changeTodoIdx
  , currTodoIdxL
  , getTodosL
  , mkCompletedTodos
  , mkNotCompletedTodos
  ) where

import Brick.Types          (Padding (Pad), Widget)

import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (ifolded, makeLenses, to, withIndex, (^.), (^..))

import Miku.Templates.Log   (Todo, todoDescL)

import Miku.Draw            (Draw (..), Drawable (..))
import Miku.Draw.StatusLine (StatusLineInfo (..))
import Miku.Resource        (Res)

import Relude

data Completed
data NotCompleted
data Todos a
  = Todos
      { _currTodoIdxL :: Int
      , _getTodosL    :: [Todo]
      }
  | NoTodos

makeLenses ''Todos

mkCompletedTodos :: Int -> [Todo] -> Todos Completed
mkCompletedTodos _ []    = NoTodos
mkCompletedTodos n todos = Todos (mod n $ length todos) todos

mkNotCompletedTodos :: Int -> [Todo] -> Todos NotCompleted
mkNotCompletedTodos _ []    = NoTodos
mkNotCompletedTodos n todos = Todos (mod n $ length todos) todos

changeTodoIdx :: Int -> Todos a -> Todos a
changeTodoIdx _ NoTodos           = NoTodos
changeTodoIdx n (Todos idx todos) = Todos (mod (n + idx) $ length todos) todos

instance StatusLineInfo (Todos Completed) where
  statusLineInfo Todos {..} = ["Todos", "Completed", "[" <> show _currTodoIdxL <> "]"]
  statusLineInfo NoTodos    = ["Todos", "Completed"]

instance StatusLineInfo (Todos NotCompleted) where
  statusLineInfo Todos {..} = ["Todos", "NotCompleted", "[" <> show _currTodoIdxL <> "]"]
  statusLineInfo NoTodos    = ["Todos", "NotCompleted"]

instance Drawable Draw (Todos Completed) where
  draw x@Draw {..} = Core.withBorderStyle _borderTypeL $ drawTodos "[✓] Completed" x

drawTodos :: Text -> Draw (Todos a) -> Widget Res
drawTodos heading Draw {..} = case _drawableL of
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

instance Drawable Draw (Todos NotCompleted) where
  draw x@Draw {..} = Core.withBorderStyle _borderTypeL $ drawTodos "[✕] Not Completed" x

drawTodo :: Todo -> Widget Res
drawTodo todo =
  Core.padBottom (Pad 1) $ Core.hBox [Core.padLeft (Pad 1) $ Core.txt $ todo ^. todoDescL]
