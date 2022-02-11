{-# LANGUAGE OverloadedStrings #-}
module Main where

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


main :: IO ()
main = do
  time <- currTime
  runMaybeT $ updateTaskIO "stuff/test.md" 0 ((timeEndL |. taskTimeL |. taskHeadingL) ?~ time)
  return ()
