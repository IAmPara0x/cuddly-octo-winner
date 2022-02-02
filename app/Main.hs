module Main where

import System.IO
import Parser
import Types

main :: IO ()
main = do
  str <- readFile "stuff/test.md"
  print $ runParser tasksP str
  fileH <- openFile "stuff/test.md" WriteMode
  hPutStr fileH $ put x
  hClose fileH
  print "Yuno"
