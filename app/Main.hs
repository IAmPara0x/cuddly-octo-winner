module Main where

import System.IO
import Task

main :: IO ()
main = do
        fileH <- openFile "stuff/test.md" WriteMode
        hPrint fileH x
        hClose fileH
