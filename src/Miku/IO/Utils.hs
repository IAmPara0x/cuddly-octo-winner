{-# LANGUAGE OverloadedStrings #-}

module Miku.IO.Utils ( currTime
                     , currDate
                     ) where


import Relude
import System.IO

import Data.Time(Day)
import Data.Time.LocalTime

import Control.Lens (Lens')

import Miku.Data.Time


currTime :: IO Time
currTime = do
           time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
           return $ newTime (todHour time) (todMin time)

currDate :: IO Day
currDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime
