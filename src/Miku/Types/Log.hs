module Miku.Types.Log (Log(Log), readLog) where

import Text.Show(Show)
import Data.Text (pack)

import Relude

import Miku.Types.Parser

import Miku.Types.Heading (Heading(Heading), headingP)
import Miku.Types.Task    (Task(Task), taskP, taskSep)

data Log = Log { logHeading :: Heading
               , logTasks   :: [Task]
               } deriving (Show)


logP :: Parser Log
logP = do
  heading <- headingP
  void (many eol)
  tasks   <- sepBy taskP taskSep
  void eof
  return $ Log heading tasks

readLog :: IO Log
readLog = do
  input <- pack <$> readFile "dailyLog.md"
  
  print input

  -- parseTest logP input

  case runParser logP "dailyLog.md" input of
    Left a    -> error $ show a
    Right log -> print log >> return log
