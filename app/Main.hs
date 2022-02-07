{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Monad.Trans.Maybe

import Control.Lens

import Types
import Crud
import Utils


main :: IO ()
main = do
  time <- currTime
  runMaybeT $ updateTaskIO "stuff/test.md" 0 ((taskTimeEndL |. headingTaskTimeL |. taskHeadingL) ?~ time)
  return ()
