module Miku.IO.Log ( newTask
                   , newLog
                   , commitLog
                   , insertTask
                   , completeTask
                   , filePath
                   , logDir
                   ) where

import Relude hiding (put)

import Control.Lens( _last
                   , (%~)
                   , (^.)
                   , (.~)
                   , (?~)
                   , (&)
                   )

import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)

import Miku.Data.Log (Log)
import Miku.Data.Task (Task)
import Miku.Data.TaskTime
import Miku.Data.Tags

import qualified Miku.Data.Log as Log
import qualified Miku.Data.Task as Task
import qualified Miku.Data.Config as Config

import Miku.IO.Config
import Miku.IO.Utils

import Parser
import Types


newTask :: Text -> Text -> [Text] -> IO Task
newTask title desc tags = do
                            time <- currTime
                            return $ Task.newTask title
                                   (newTaskTime time) desc (fromList tags)

newLog :: EitherIO String
newLog = do
           config    <- readConfig
           path      <- newFilePath
           case config ^. Config.filePathL of
             Nothing  -> writeConfig (config & Config.filePathL ?~ path)
                      >> lift currDate >>= writeLog path . Log.newLog
             (Just _) -> throwE "There's already log created."

readLog :: FilePath -> EitherIO Log
readLog f = do
              str <- readFileText f
              case fst <$> runParser parse str of
                Nothing    -> throwE ("Was not able to read logs from " ++ f)
                (Just log) -> return log

writeLog :: FilePath -> Log -> EitherIO String
writeLog f log = lift (writeFileText f $ put log)
              >> return "Successfully written the log."

commitLog :: EitherIO String
commitLog = do
              config <- readConfig
              case config ^. Config.filePathL of
                Nothing  -> throwE "There's not log file to commit."
                (Just f) -> writeConfig (config & Config.filePathL .~ Nothing)
                         >> return "log file has been commited."

insertTask :: FilePath -> Task -> EitherIO String
insertTask f task = readLog f >>= writeLog f . Log.insertTask task
                 >> return "Inserted the task."
                        
completeTask :: FilePath -> EitherIO String
completeTask f = do
                   time <- lift currTime
                   log  <- update time <$> readLog f
                   writeLog f log
                   return "Current Task has been marked as completed."
  where
    update t = Log.logTasksL . _last %~ Task.completeTask t

