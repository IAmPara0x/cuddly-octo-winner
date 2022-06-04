module Miku.Types.Task ( Task(Task)
                       , taskName
                       , taskStart
                       , taskEnd
                       , taskSep
                       ) where

import qualified Data.Text as T
import           Text.Show (Show(..))
import           Relude hiding (show)

import           Miku.Types.Time
import           Miku.Types.Parser

data Task = Task { taskName  :: Text
                 , taskStart :: Time
                 , taskEnd   :: Maybe Time
                 }

taskPrefix :: Text
taskPrefix = "###"

instance Show Task where
  show = T.unpack . putElement


instance Element Task where
  parseElement = taskP
  putElement  (Task name startT Nothing) = taskPrefix
                                         <> " " <> name <> " " 
                                         <> "(" <> putElement startT <> " - )"
  putElement  (Task name startT (Just endT)) = taskPrefix
                                             <> " " <> name <> " " 
                                             <> "(" <> putElement startT
                                             <> " - " <> putElement endT <> ")"

taskP :: Parser Task
taskP = do
  void (tokenP $ string taskPrefix)

  name   <- T.strip <$> takeWhileP (Just "an alpha numeric character") (/= '(')

  void (tokenP $ char '(')

  startT <- tokenP parseElement

  void (tokenP $ char '-')

  endT   <- optional (tokenP parseElement)

  void $ tokenP (char ')') <* many eol

  return $ Task name startT endT

taskSep :: Parser Text
taskSep = string "---" <* many eol
