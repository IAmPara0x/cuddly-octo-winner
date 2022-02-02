{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as IO
import Parser
import Types

main :: IO ()
main = do
  str <- IO.readFile "stuff/test.md"
  print $ runParser tasksP str
  print "Yuno"
