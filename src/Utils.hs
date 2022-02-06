module Utils ( currTime
             , updateTaskTime
             ) where

import Data.Time.LocalTime
import Lens.Micro ( (?~)
                  )

import Types


currTime :: IO Time
currTime = do
           time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
           return $ newTime (todHour time) (todMin time)

updateTaskTime :: Time -> Task -> Task
updateTaskTime time = (taskHeadingL . headingTaskTimeL . taskTimeEndL) ?~ time

