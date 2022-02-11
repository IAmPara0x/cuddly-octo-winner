{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.TaskTime ( TaskTime(TaskTime)
                          , timeStartL
                          , timeEndL
                          , taskTimeP
                          )
                          where

import Relude hiding (put)
import qualified Data.Text as T

import Data.Maybe (fromJust)
import Control.Lens

import Parser
import Types
import Syntax

import Miku.Data.Time

data TaskTime = TaskTime { _timeStartL :: Time
                         , _timeEndL :: Maybe Time
                         }
                deriving (Show)

makeLenses ''TaskTime

-- Syntax
--

taskTimePrefix :: Char
taskTimePrefix  = '('

taskTimeSuffix :: Char
taskTimeSuffix  = ')'

taskTimeSep :: Char
taskTimeSep = '-'


instance Put TaskTime where
  put (TaskTime start end)
    | isNothing end = elem $ T.concat [put start <+ 2,
                                       T.singleton taskTimeSep <+ 2
                                      ]
    | otherwise     = elem $ T.concat [put start <+ 2,
                                       T.singleton taskTimeSep <+ 2,
                                       put (fromJust end)
                                      ]
    where
      elem = surroundElem (T.singleton taskTimePrefix) (T.singleton taskTimeSuffix)


taskTimeP :: Parser TaskTime
taskTimeP = do
              symbCharP taskTimePrefix
              startT <- tokenP timeP
              symbCharP taskTimeSep
              endT <- (Just <$> tokenP timeP <* symbCharP taskTimeSuffix) `mplus`
                      (Nothing <$ symbCharP taskTimeSuffix)
              return (TaskTime startT endT)

