module Miku.IO.Crud ( readLog
                    , writeLog
                    , newTask
                    , newLog
                    , insertTask
                    , completeTask
                    ) where

import Relude hiding (put)

import Control.Lens( _last
                   , (%~)
                   , (&)
                   )

import Control.Monad.Trans.Except
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


type FileName   = String
type EitherIO a = ExceptT String IO a

readLog :: FileName -> EitherIO Log
readLog f = do
              str <- readFileText f
              case fst <$> runParser parse str of
                Nothing    -> throwE ("Was not able to read logs from " ++ f)
                (Just log) -> except $ Right log

writeLog :: FileName -> Log -> IO()
writeLog f = writeFileText f . put

newTask :: Text -> Text -> [Text] -> IO Task
newTask title desc tags = do
                            time <- currTime
                            return $ Task.newTask title
                                   (newTaskTime time) desc (fromList tags)
newLog :: IO Log
newLog = Log.newLog <$> currDate

insertTask :: FileName -> Task -> EitherIO ()
insertTask f task = readLog f >>= lift . writeLog f . Log.insertTask task >> return ()
                        
completeTask :: FileName -> EitherIO ()
completeTask f = do
                   time <- lift currTime
                   log  <- update time <$> readLog f
                   lift $ writeLog f log
                   return ()
  where
    update t = Log.logTasksL . _last %~ Task.completeTask t
