{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Monad.Trans.Maybe

import Utils


title = "New Title V3"
desc = "New Desc v2 with time.\n\t with some tabs."
tags = ["Yuno", "Gasai"]


main :: IO ()
main = do
  tasks <- runMaybeT $ fileTasksP "stuff/test.md"
  case tasks of
    Nothing -> print "Error: Parsing Error."
    Just value -> print value
  -- time <- currTime
  -- hfile <- openFile "stuff/test.md" AppendMode
  -- IO.hPutStr hfile $ put $ newTask title (TaskTime time Nothing) "" tags
  -- case updateTaskTime time . head . fst <$> runParser tasksP str of
  --   Nothing -> print "lol"
  --   (Just task) -> IO.hPutStr hfile $ put task
  -- hClose hfile

