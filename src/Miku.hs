{-# Language FlexibleContexts #-}

module Miku ( go
            )
            where


import Relude

import Control.Monad (void)
import Control.Monad.Except (runExceptT)

import Miku.IO.Log
import Miku.IO.Types
import Miku.IO.Config

x = newTask "This is a newTask v3" "This is a new description 3." ["Miku", "Yuno", "Gasai"]
-- 
-- n = newLog
d = logPath >>= completeTask
-- i = filePath >>= (\f -> lift task >>= insertTask f)
-- c = commitLog

go :: IO()
go = do
       x' <- x
       eitherV <- runExceptT (commitLog)
       case eitherV of
         (Left  e) -> print e
         (Right m) -> print (m::(Msg Log))
-- go = print "Yuno"
