{-# LANGUAGE OverloadedStrings #-}

module Miku.IO.Utils ( currTime
                     , currDate
                     , EitherIO
                     , mCall
                     ) where


import Relude
import System.IO

import Data.Time(Day)
import Data.Time.LocalTime

import Control.Lens (Lens')

import Miku.Data.Time

type EitherIO a = ExceptT String IO a

currTime :: IO Time
currTime = do
           time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
           return $ newTime (todHour time) (todMin time)

currDate :: IO Day
currDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime

mCall :: Maybe a -> (a -> EitherIO b) -> EitherIO b -> EitherIO b
mCall Nothing _ v  = v
mCall (Just a) f _ = f a
