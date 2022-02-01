{-# LANGUAGE OverloadedStrings #-}

module Types ( Heading(..)
             , TaskTime(..)
             , Desc(..)
             , Tags(..)
             , Task(..)
             , Put(..)
             ) where

import Data.Maybe ( fromJust
                  , isNothing
                  )
import Syntax
import Time (Time)


class (Put a) where
  put :: a -> String


data Heading = Heading String TaskTime
               deriving (Show)


instance Put Heading where
  put (Heading name time) = concat [headingPrefix <+ 2, name <+ 2,
                                    put time, headingSuffix, newline 2
                                   ]


data TaskTime = TaskTime Time (Maybe Time)
                deriving (Show)


instance Put TaskTime where
  put (TaskTime start end)
    | isNothing end = concat [taskTimePrefix:[], show start <+ 2,
                              taskTimeSep:[] <+ 2, taskTimeSuffix:[]
                             ]
    | otherwise     = concat [taskTimePrefix:[], show start <+ 2, taskTimeSep:[] <+ 2 ,
                              show (fromJust end), taskTimeSuffix:[]
                             ]


newtype Tags = Tags [String]
               deriving (Show)

instance Put Tags where
  put (Tags (tag:tags)) = concat [2 +> tagsPrefix, 2 +> tagsStr, tagsSuffix]
    where
      tagsStr = tag ++ foldMap ((tagsSep:wSpace 1) ++) tags


newtype Desc = Desc String
               deriving (Show)

instance Put Desc where
  put (Desc str) = concat [2 +> descPrefix, 2 +> str, descSuffix, newline 2]


data Task = Task { taskHeading :: Heading
                 , taskDesc :: Maybe Desc
                 , taskTags :: Tags
                 } deriving (Show)


instance Put Task where
  put (Task heading Nothing tags)     = concat [put heading, put tags, newline 2]
  put (Task heading (Just desc) tags) = concat [put heading, put desc,
                                                put tags, newline 2
                                               ]
