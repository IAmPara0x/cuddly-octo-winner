{-# Language TemplateHaskell #-}

module Miku.UI.State ( State(..)
                     , newState
                     ) where

import Relude hiding (State)
import Control.Monad (liftM2)
import Control.Monad.Trans.Except (throwE, runExceptT)

import Control.Lens (makeLenses)

import Miku.IO.Config
import Miku.IO.Log

import Miku.IO.Types

data State = State { _stateLogL     :: Either (Msg Log) Log
                   , _stateLogPathL :: Either (Msg LogPath) LogPath
                   } deriving (Show)

makeLenses ''State

newState :: IO State
newState = do
             log  <- runExceptT readL
             path <- runExceptT readL
             return (State log path)

-- instance CmdL State where
--   prefixMsg Err = "State Error: "
--   prefixMsg Suc = "State Success: "
-- 
--   newM          = createM (liftM2 State (runExceptT readL) (runExceptT readL))
--                 $ msg Suc "created new state."
--   readM         = createM (throwE $ msg Err "`readM` is not implmented for `State`.") Err
--   writeM _      = createM (throwE $ msg Err "`readM` is not implmented for `State`.") Err



