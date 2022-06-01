module Miku.Types.Task (Task(Task), taskP, taskSep) where

import qualified Data.Text as Text
import Relude

import Miku.Types.Time
import Miku.Types.Parser

data Task = Task { taskName  :: Text
                 , taskStart :: Time
                 , taskEnd   :: Maybe Time
                 } deriving (Show)

taskP :: Parser Task
taskP = do
  void (tokenP $ string "###")

  name   <- Text.strip <$> takeWhileP (Just "an alpha numeric character") (/= '(')

  void (tokenP $ char '(')

  startT <- tokenP timeP

  void (tokenP $ char '-')

  endT   <- optional (tokenP timeP)

  void $ tokenP (char ')') <* (many eol)

  return $ Task name startT endT

taskSep :: Parser Text
taskSep = string "---" <* (many eol)
