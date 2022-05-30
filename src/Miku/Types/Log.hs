module Miku.Types.Log (Log(Log), readLog) where

import Text.Show(Show)
import Data.Text (pack)

import Relude

import Miku.Types.Parser

import Miku.Types.Heading (Heading(Heading), headingP)

data Log = Log { logHeading :: Heading
               } deriving (Show)


logP :: Parser Log
logP = do
  heading <- headingP
  void eof
  return $ Log heading

readLog :: IO Log
readLog = do
  input <- pack <$> readFile "dailyLog.md"
  print input


  case runParser logP "dailyLog.md" input of
    Left a    -> error $ show a
    Right log -> return log
