module Miku ( go
            )
            where


import Relude

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Miku.IO.Crud

task = newTask "This is a newTask v3" "This is a new description 3." ["Miku", "Yuno"]

go :: IO()
-- go = newLog >>= writeLog "stuff/test.md"
-- go = task >>= runExceptT . insertTask "stuff/test.md" >> return ()
go = void $ runExceptT (completeTask "stuff/test.md")
-- go = do
--        eitherV <- runExceptT $ readLog "stuff/test.md"
--        case eitherV of
--          (Left e) -> print e
--          (Right _) -> print "success"
--
--
