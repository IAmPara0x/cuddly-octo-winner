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

taskTimePrefix :: Text
taskTimePrefix  = "("

taskTimeSuffix :: Text
taskTimeSuffix  = ")"

taskTimeSep :: Text
taskTimeSep = "-"


instance Element TaskTime where
  prefix            = const $ Just taskTimePrefix

  sep (TaskTime start end)
    | isNothing end = Just $ T.concat [put start <+ 2,
                                       taskTimeSep <+ 2
                                      ]
    | otherwise     = Just $ T.concat [put start <+ 2,
                                       taskTimeSep <+ 2,
                                       put (fromJust end)
                                      ]

  suffix            = const $ Just taskTimeSuffix
  parse             = taskTimeP


taskTimeP :: Parser TaskTime
taskTimeP = do
              symbP taskTimePrefix
              startT <- tokenP parse
              symbP taskTimeSep
              endT <- (Just <$> tokenP parse <* symbP taskTimeSuffix) `mplus`
                      (Nothing <$ symbP taskTimeSuffix)
              return (TaskTime startT endT)

