{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import qualified Data.Text.IO as IO

import Parser
import Types
import Utils


title = "New Title V2"
desc = "New Desc v2 with time.\n\t with some tabs."
tags = ["Yuno", "Gasai"]


main :: IO ()
main = do
  str <- IO.readFile "stuff/test.md"
  time <- currTime
  hfile <- openFile "stuff/test.md" WriteMode
  case updateTaskTime time . head . fst <$> runParser tasksP str of
    Nothing -> print "lol"
    (Just task) -> IO.hPutStr hfile $ put task
  hClose hfile

