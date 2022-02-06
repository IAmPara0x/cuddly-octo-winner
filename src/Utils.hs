module Utils ( currTime
             , updateTaskTime
             , fileTasksP
             ) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import System.IO
import qualified Data.Text.IO as IO
import qualified Data.Text as T

import Data.Time.LocalTime
import Lens.Micro ( (?~)
                  )

import Parser
import Types


currTime :: IO Time
currTime = do
           time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
           return $ newTime (todHour time) (todMin time)

updateTaskTime :: Time -> Task -> Task
updateTaskTime time = (taskHeadingL . headingTaskTimeL . taskTimeEndL) ?~ time

fileTasksP :: String -> MaybeT IO [Task]
fileTasksP fName = MaybeT $
                        do
                          str <- IO.readFile fName
                          return $ fst <$> runParser tasksP str


f = do
    time <- lift currTime
    tasks <- fileTasksP "stuff/test.md"
    return $ map (updateTaskTime time) tasks
