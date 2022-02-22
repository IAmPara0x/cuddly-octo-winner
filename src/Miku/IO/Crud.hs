module Miku.IO.Crud ( readLog
                    , writeLog
                    , newTask
                    , newLog
                    , insertTask
                    ) where

import Relude hiding (put)

import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift)

import qualified Data.Text as T

import Miku.Data.Log (Log)
import Miku.Data.Task (Task)
import Miku.Data.TaskTime
import Miku.Data.Tags

import qualified Miku.Data.Log as Log
import qualified Miku.Data.Task as Task

import Miku.IO.Utils

import Parser
import Types

type FileName = String

readLog :: FileName -> MaybeT IO Log
readLog fName = MaybeT $
                    do
                      str <- readFileText fName
                      return $ fst <$> runParser parse str

writeLog :: FileName -> Log -> IO()
writeLog fName = writeFileText fName . put

newTask :: Text -> Text -> [Text] -> IO Task
newTask title desc tags = do
                           time <- currTime
                           return $ Task.newTask title
                                   (newTaskTime time) desc (fromList tags)
newLog :: IO Log
newLog = Log.newLog <$> currDate

insertTask :: FileName -> Task -> MaybeT IO ()
insertTask f task = readLog f >>= lift . writeLog f . Log.insertTask task >> return ()
                        
