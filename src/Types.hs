{-# LANGUAGE OverloadedStrings #-}

module Types ( Heading(..)
             , TaskTime(..)
             , Desc(..)
             , Tags(..)
             , Task(..)
             , Put(..)
             ) where

import qualified Data.Text as T
import Data.Maybe ( fromJust
                  , isNothing
                  )
import Syntax
import Time (Time)


class (Put a) where
  put :: a -> T.Text


data Heading = Heading T.Text TaskTime
               deriving (Show)


instance Put Heading where
  put (Heading name time) = T.concat [headingPrefix <+ 2, name <+ 2,
                                      put time, headingSuffix, newline 2
                                     ]


data TaskTime = TaskTime Time (Maybe Time)
                deriving (Show)


instance Put TaskTime where
  put (TaskTime start end)
    | isNothing end = T.concat [T.singleton taskTimePrefix, (T.pack $ show start) <+ 2,
                                (T.singleton taskTimeSep) <+ 2, T.singleton taskTimeSuffix
                               ]
    | otherwise     = T.concat [T.singleton taskTimePrefix, (T.pack $ show start) <+ 2,
                                (T.singleton taskTimeSep) <+ 2 , (T.pack $ show (fromJust end)),
                                T.singleton taskTimeSuffix
                               ]


newtype Tags = Tags [T.Text]
               deriving (Show)

instance Put Tags where
  put (Tags (tag:tags)) = T.concat [2 +> tagsPrefix, 2 +> tagsStr, tagsSuffix]
    where
      tagsStr = T.append tag $ foldMap (T.append ", ") tags


newtype Desc = Desc T.Text
               deriving (Show)

instance Put Desc where
  put (Desc str) = T.concat [2 +> descPrefix, 2 +> str, descSuffix, newline 2]


data Task = Task { taskHeading :: Heading
                 , taskDesc :: Maybe Desc
                 , taskTags :: Tags
                 } deriving (Show)


instance Put Task where
  put (Task heading Nothing tags)     = T.concat [put heading, put tags,
                                                  newline 2, taskSep
                                                 ]
  put (Task heading (Just desc) tags) = T.concat [put heading, put desc, put tags,
                                                  newline 2, taskSep, newline 2
                                                 ]
