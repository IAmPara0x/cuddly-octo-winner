{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Utils ( currTime
             , (|.)
             ) where


import System.IO
import Data.Time.LocalTime

import Control.Lens (Lens')

import Types


(|.) :: Lens' b c -> Lens' a b -> Lens' a c
(|.) = flip (.)


currTime :: IO Time
currTime = do
           time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
           return $ newTime (todHour time) (todMin time)
