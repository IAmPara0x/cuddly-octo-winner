{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as IO
import System.IO
import Parser
import Types


x = Task { _taskHeadingL = Heading "New Task" $ TaskTime (newTime 10 20) Nothing
         , _taskDescL    = Just $ Desc "new description of new task"
         , _taskTagsL    = Tags ["Yuno", "Gasai", "I Am Paradox"]
         }

main :: IO ()
main = do
  str <- IO.readFile "stuff/test.md"
  print $ runParser tasksP str
  hfile <- openFile "stuff/test.md" AppendMode
  -- IO.hPutStr hfile $ put x
  hClose hfile
  print "Yuno"
