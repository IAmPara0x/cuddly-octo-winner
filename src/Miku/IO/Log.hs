module Miku.IO.Log ( newTask
                   , newLog
                   , readLog
                   , writeLog
                   , commitLog
                   , completeTask
                   , writeTask
                   , readTask
                   , Log
                   , Task
                   ) where

import Relude hiding (put)

import Control.Lens( _last
                   , (%~)
                   , (^.)
                   , (.~)
                   )
import Data.Sequence ( ViewR(..)
                     , viewr
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
import Miku.IO.Types

import Miku.Data.Parser
import Miku.Data.Types



-- Commands for Log

newLog :: EitherIO Log
newLog = do
           config    <- readL
           path      <- newLogPath
           case config ^. Config.logFPathL of
             Nothing  -> writeLogPath path
                      >> lift currDate >>= (writeL . Log.newLog)
             (Just _) -> throwE (msg Err "There's already log created.")

readLog :: FilePath -> EitherIO Log
readLog f = do
              log <- toValue <$> readFileText f
              mCall log return (throwE $ msg Err "unable to parse the tasks from current log file.")

writeLog :: FilePath -> Log -> EitherIO Log
writeLog f log = do
                   lift (writeFileText f $ put log)
                   return log

commitLog :: EitherIO (Msg Log)
commitLog = do
              config <- readL
              case config ^. Config.logFPathL of
                Nothing  -> throwE $ msg Err "There's not current log to commit."
                (Just f) -> writeL (config & Config.logFPathL .~ Nothing)
                         >> return (msg Suc "commited the current log.")

instance CmdL Log where
  readM      = createM (logPath >>= readLog) (msg Suc "parsed tasks from current log.")
  newM       = createM newLog  (msg Suc "created new empty log file.")
  writeM log = createM (logPath >>= (`writeLog` log)) (msg Suc "written the current log.")


-- Commands for task

newTask :: Text -> Text -> [Text] -> IO Task
newTask title desc tags = do
                            time <- currTime
                            return $ Task.newTask title
                                   (newTaskTime time) desc (fromList tags)

readTask :: FilePath -> EitherIO Task
readTask f = do
               log <- readLog f
               case viewr (log ^. Log.logTasksL) of
                 _ :> task -> return task
                 _         -> throwE (msg Err $ "There's no task to read from the log: " <> f)


writeTask :: FilePath -> Task -> EitherIO Task
writeTask f task = readLog f >>= writeLog f . Log.insertTask task
                  >> return task

instance CmdL Task where
  readM       = createM (logPath >>= readTask) (msg Suc "read the last task from the current log.")
  writeM task = createM (logPath >>= (`writeTask` task)) (msg Suc "inserted the task.")
  newM        = error "Error: `newM` is not implemented for the `Task` type."
  
  
completeTask :: FilePath -> EitherIO (Msg Task)
completeTask f = do
                   time <- lift currTime
                   log  <- update time <$> readLog f
                   writeLog f log
                   return (msg Suc "Current Task has been marked as completed.")
  where
    update t = Log.logTasksL . _last %~ Task.completeTask t
