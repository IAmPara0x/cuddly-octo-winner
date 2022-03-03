module Miku.IO.Log ( newTask
                   , newLog
                   , readLog
                   , writeLog
                   , commitLog
                   , completeTask
                   , completeTaskM
                   , commitLogM
                   , writeTask
                   , readTask
                   , Log
                   , Task
                   , Config.Config
                   , LogPath
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
import Miku.Data.Config (LogPath)
import Miku.Data.TaskTime

import qualified Miku.Data.Log as Log
import qualified Miku.Data.Task as Task
import qualified Miku.Data.Config as Config

import Miku.IO.Config
import Miku.IO.Utils
import Miku.IO.Types

import Miku.Data.Types



-- Commands for Log

newLog :: EitherIO Log
newLog = do
           config    <- readL
           case config ^. Config.logFPathL of
             Nothing  -> (newL::(EitherIO LogPath))
                      >> lift currDate >>= (writeL . Log.newLog)
             (Just _) -> throwE (msg Err "There's already log created.")

readLog :: LogPath -> EitherIO Log
readLog f = do
              log <- toValue <$> readFileText (toString f)
              mCall log return (throwE $ msg Err "unable to parse the tasks from current log file.")

writeLog :: LogPath -> Log -> EitherIO Log
writeLog f log = do
                   lift (writeFileText (toString f) $ put log)
                   return log

commitLog :: EitherIO Log
commitLog = do
              config <- readL
              case config ^. Config.logFPathL of
                Nothing  -> throwE $ msg Err "There's not current log to commit."
                (Just f) -> writeL (config & Config.logFPathL .~ Nothing)
                         >> readLog f

commitLogM :: Miku Log
commitLogM = createM commitLog (msg Suc "commited the current log.")

instance CmdL Log where
  prefixMsg Err = "Log Error: "
  prefixMsg Suc = "Log Success: "
  
  readM      = createM (readL >>= readLog)
             $ msg Suc "parsed tasks from current log."

  newM       = createM newLog 
             $ msg Suc "created new empty log file."

  writeM log = createM (readL >>= (`writeLog` log))
             $ msg Suc "written the current log."


-- Commands for task

newTask :: Text -> Text -> [Text] -> IO Task
newTask title desc tags = do
                            time <- currTime
                            return $ Task.newTask title
                                   (newTaskTime time) desc (fromList tags)

readTask :: LogPath -> EitherIO Task
readTask f = do
               log <- readLog f
               case viewr (log ^. Log.logTasksL) of
                 _ :> task -> return task
                 _         -> throwE (msg Err $ "There's no task to read from the log: " <> toString f)


writeTask :: LogPath -> Task -> EitherIO Task
writeTask f task = readLog f >>= writeLog f . Log.insertTask task
                  >> return task

instance CmdL Task where
  prefixMsg Err = "Task Error: "
  prefixMsg Suc = "Task Success: "

  readM       = createM (readL >>= readTask)
              $ msg Suc "read the last task from the current log."

  writeM task = createM (readL >>= (`writeTask` task))
              $ msg Suc "inserted the task."

  newM        = createM (throwE $ msg Err "`newM` is not implemented for the `Task` type.") Err
  
completeTask :: LogPath -> EitherIO Task
completeTask f = do
                   time <- lift currTime
                   log  <- update time <$> readLog f
                   writeLog f log >> readL
  where
    update t = Log.logTasksL . _last %~ Task.completeTask t

completeTaskM :: Miku Task
completeTaskM = createM (readL >>= completeTask)
              $ msg Suc "Current Task has been marked as completed."

