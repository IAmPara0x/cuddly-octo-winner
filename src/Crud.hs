module Crud ( newTaskIO
            , updateTaskIO
            , readTasksIO
            ) where

import Relude hiding (put)

import System.IO

import qualified Data.Text.IO as IO
import qualified Data.Text as T

import Control.Monad (mapM_)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Lens.Traversal (element)
import Control.Lens ( (%~)
                    , (.~)
                    )

import Parser
import Types
import Miku.Data.Task
import Miku.Data.Time


type FileName = String

readTasksIO :: FileName -> MaybeT IO [Task]
readTasksIO fName = MaybeT $
                        do
                          str <- IO.readFile fName
                          return $ fst <$> runParser tasksP str

newTaskIO :: FileName -> Task -> MaybeT IO ()
newTaskIO fName task = do
                         tasks <- readTasksIO fName
                         hfile <- lift $ openFile fName AppendMode
                         lift $ IO.hPutStrLn hfile $ put task
                         lift $ hClose hfile


updateTaskIO :: FileName -> Int -> (Task -> Task) -> MaybeT IO ()
updateTaskIO fName idx f = do
                             tasks <- (element idx %~ f) <$> readTasksIO fName
                             hfile <- lift $ openFile fName WriteMode
                             lift $ mapM_ (IO.hPutStr hfile . put) tasks
                             lift $ hClose hfile
