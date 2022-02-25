module Miku ( go
            )
            where


import Relude

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Miku.IO.Log

task = newTask "This is a newTask v3" "This is a new description 3." ["Miku", "Yuno"]

n = newLog
d = filePath >>= completeTask
i = filePath >>= (\f -> lift task >>= insertTask f)
c = commitLog

go :: IO()
go = do
       eitherV <- runExceptT d
       case eitherV of
         (Left  e) -> print e
         (Right m) -> print m
