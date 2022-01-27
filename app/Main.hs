module Main where

import System.IO
import Parser
import Control.Applicative

main :: IO ()
main = do
        newX <- readFile "stuff/test.md"
        print $ runParser taskP newX

