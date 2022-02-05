{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as IO
import System.IO
import Control.Monad (liftM)
import Data.Time.LocalTime
import Parser
import Types
import Lens.Micro ((^.))


f time = Task { _taskHeadingL = Heading "Updated Task" $ TaskTime time Nothing
              , _taskDescL    = Just $ Desc "new description of new task"
              , _taskTagsL    = Tags ["Yuno", "Gasai", "I Am Paradox", "Real Analysis"]
              }

main :: IO ()
main = do
  str <- IO.readFile "stuff/test.md"
  print $  map (^. taskTagsL) . fst <$> runParser tasksP str
  time <- fmap (localTimeOfDay . zonedTimeToLocalTime) getZonedTime
  hfile <- openFile "stuff/test.md" WriteMode
  IO.hPutStr hfile $ put (f $ newTime (todHour time) (todMin time))
  hClose hfile
  print "Yuno"
