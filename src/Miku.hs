module Miku ( go
            )
            where

import Relude

import Control.Monad.Trans
import Miku.IO.Crud

task = newTask "This is a newTask v3" "This is a new description." ["Miku"]

go :: IO()
go = task >>= runMaybeT . insertTask "stuff/test.md" >> return ()
