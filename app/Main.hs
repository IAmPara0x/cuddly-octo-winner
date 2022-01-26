module Main where

import System.IO
import Parser


main :: IO ()
main = do
        fileH <- openFile "stuff/test.md" WriteMode
        hPrint fileH x
        hClose fileH
