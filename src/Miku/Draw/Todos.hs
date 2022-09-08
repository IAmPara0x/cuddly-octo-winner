{-# LANGUAGE NamedFieldPuns #-}
module Miku.Draw.Todos
  ( CompletedTodos (..)
  , NotCompletedTodos (..)
  , _CompletedTodos
  , _NotCompletedTodos
  ) where

import Brick.Types          (Padding (Pad), Widget)

import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Center qualified as Core
import Brick.Widgets.Core   qualified as Core

import Control.Lens         (ifolded, makePrisms, to, withIndex, (^.), (^..))

import Miku.Templates.Log   (Todo, todoDescL)

import Miku.Draw            (Draw (..), Drawable (..))
import Miku.Mode            (Name)

import Relude

data CompletedTodos    = CompletedTodos Int [Todo]
makePrisms ''CompletedTodos

data NotCompletedTodos = NotCompletedTodos Int [Todo]
makePrisms ''NotCompletedTodos

instance Drawable CompletedTodos where
  draw Draw{ _drawableL=CompletedTodos currIdx todos , .. }
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

instance Drawable NotCompletedTodos where
  draw Draw{ _drawableL=NotCompletedTodos currIdx todos , .. }
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
