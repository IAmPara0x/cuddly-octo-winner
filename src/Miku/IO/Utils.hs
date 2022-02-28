{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# Language GADTs #-}

module Miku.IO.Utils ( currTime
                     , currDate
                     , mCall
                     ) where


import Relude
import System.IO

import Data.Time(Day)
import Data.Time.LocalTime

import Miku.Data.Time

import Miku.IO.Types


currTime :: IO Time
currTime = do
           time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
           return $ newTime (todHour time) (todMin time)

currDate :: IO Day
currDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime

mCall :: Maybe a -> (a -> EitherIO b) -> EitherIO b -> EitherIO b
mCall Nothing _ v  = v
mCall (Just a) f _ = f a
