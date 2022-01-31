module Main where

import System.IO
import Parser
import Types

main :: IO ()
main = do
  fileH <- openFile "stuff/test.md" WriteMode
  hPutStr fileH (put x)
  hClose fileH
