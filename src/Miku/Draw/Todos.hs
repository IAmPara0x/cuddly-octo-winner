{-# LANGUAGE NamedFieldPuns #-}
module Miku.Draw.Todos
  ( Completed
  , NotCompleted
  , Todos (_getTodosL, _currTodoIdxL)
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
import Miku.Mode            (Name)

import Relude

data Completed
data NotCompleted
data Todos a
  = Todos
      { _currTodoIdxL :: Int
      , _getTodosL    :: [Todo]
      }

makeLenses ''Todos

mkCompletedTodos :: Int -> [Todo] -> Todos Completed
mkCompletedTodos n todos = Todos (mod n $ length todos) todos

mkNotCompletedTodos :: Int -> [Todo] -> Todos NotCompleted
mkNotCompletedTodos n todos = Todos (mod n $ length todos) todos

changeTodoIdx :: Int -> Todos a -> Todos a
changeTodoIdx n (Todos idx todos) = Todos (mod (n + idx) $ length todos) todos

instance StatusLineInfo (Todos Completed) where
  statusLineInfo Todos{..} = ["Todos", "Completed", show _currTodoIdxL]

instance StatusLineInfo (Todos NotCompleted) where
  statusLineInfo Todos{..} = ["Todos", "NotCompleted", show _currTodoIdxL]

instance Drawable (Todos Completed) where
  draw Draw{ _drawableL=Todos currIdx todos , .. }
    = Core.withBorderStyle _borderTypeL $ drawTodos "[✓] Completed"

    where drawTodos heading =
            Border.border
            $ Core.center
            $ Core.vBox
                [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
                , Core.vBox
                    (todos ^.. ifolded . withIndex . to addAttr)
                , Core.fill ' '
                ]

          addAttr (idx, todo)
              | not _focusedL  = Core.withAttr "todo"    $ drawTodo todo
              | idx == currIdx = Core.withAttr "current" $ drawTodo todo
              | otherwise      = drawTodo todo
--
instance Drawable (Todos NotCompleted) where
  draw Draw{ _drawableL=Todos currIdx todos , .. }
    = Core.withBorderStyle _borderTypeL $ drawTodos "[✕] Not Completed"
    where drawTodos heading =
            Border.border
            $ Core.center
            $ Core.vBox
                [ Core.padTopBottom 1 $ Core.hCenter $ Core.txt heading
                , Core.vBox
                    (todos ^.. ifolded . withIndex . to addAttr)
                , Core.fill ' '
                ]

          addAttr (idx, todo)
              | not _focusedL  = Core.withAttr "todo"    $ drawTodo todo
              | idx == currIdx = Core.withAttr "current" $ drawTodo todo
              | otherwise      = drawTodo todo

drawTodo :: Todo -> Widget Name
drawTodo todo = Core.padBottom (Pad 1) $
  Core.hBox [ Core.padLeft (Pad 1)
            $ Core.txt $ todo ^. todoDescL
            ]
