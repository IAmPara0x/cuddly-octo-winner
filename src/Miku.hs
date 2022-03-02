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

x = newTask "This is a newTask v3" "This is a new description 3." ["Miku", "Yuno"]
-- 

type T = Msg Task

go :: IO()
go = do
       x' <- x
       eitherV <- runExceptT (execM $ completeTaskM)
       case eitherV of
         (Left  e) -> print (e :: T)
         (Right m) -> print (m :: T)
