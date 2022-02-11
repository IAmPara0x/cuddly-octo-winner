module Miku ( go
            )
            where

import Relude

import System.IO
import Control.Monad.Trans.Maybe

import Control.Lens

import Types
import Crud
import Utils
import Miku.Data.Task
import Miku.Data.TaskTime
import Miku.Data.Heading
import Miku.Data.Time

x = newTask "This is Miku" (TaskTime (newTime 10 20) Nothing) "Just a random Desc." ["Yuno", "Gasai"]

go :: IO ()
go = do
  time <- currTime
  runMaybeT $ updateTaskIO "stuff/test.md" 0 ((timeEndL |. taskTimeL |. taskHeadingL) ?~ time)
  return ()
