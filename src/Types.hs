{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types ( Time
             , newTime
             , Heading(..)
             , headingTitleL
             , headingTaskTimeL
             , TaskTime(..)
             , tagsL
             , taskTimeStartL
             , taskTimeEndL
             , Desc(..)
             , Tags(..)
             , Task(..)
             , taskHeadingL
             , taskDescL
             , taskTagsL
             , Put(..)
             ) where

import qualified Data.Text as T
import Lens.Micro.TH (makeLenses)
import Data.Bifunctor (bimap)
import Data.Maybe ( fromJust
                  , isNothing
                  )
import Syntax


-- TODO: Find better way to print the syntax!.
class (Put a) where
  put :: a -> T.Text


mins :: Int
mins = 60

hrs :: Int
hrs = 24

data Time = Time Int Int
            deriving (Show)

newTime :: Int -> Int -> Time
newTime h m = Time hnew mnew
  where
    mnew = mod m mins
    hnew = h + div m mins

instance Put Time where
  put (Time h m) = T.concat [T.pack $ show h, timeHrs, T.pack $ show m, timeMins]

instance Eq Time where
  (==) (Time h1 m1) (Time h2 m2) = h1 == h2 && m1 == m2

instance Ord Time where
  (<=) (Time h1 m1) (Time h2 m2) = h1 <= h2 && m1 <= m2

instance Num Time where
  (+) (Time h1 m1) (Time h2 m2) = newTime (h1 + h2) (m1 + m2)
  (*) (Time h1 m1) (Time h2 m2) = newTime (h1 * h2) (m1 * m2)
  abs (Time h1 m1)              = newTime (abs h1) (abs m1)
  signum (Time h1 m1)           = error "Error: Time does not have `signum` function implemented."
  negate (Time h1 m1)           = newTime (- h1) (- m1)
  fromInteger                   = newTime 0 . fromIntegral

-- TaskTime
--
data TaskTime = TaskTime { _taskTimeStartL :: Time
                         , _taskTimeEndL :: Maybe Time
                         }
                deriving (Show)

makeLenses ''TaskTime

instance Put TaskTime where
  put (TaskTime start end)
    | isNothing end = T.concat [T.singleton taskTimePrefix, put start <+ 2,
                                T.singleton taskTimeSep <+ 2, T.singleton taskTimeSuffix
                               ]
    | otherwise     = T.concat [T.singleton taskTimePrefix, T.pack (show start) <+ 2,
                                T.singleton taskTimeSep <+ 2 , put (fromJust end),
                                T.singleton taskTimeSuffix
                               ]

-- Heading

data Heading = Heading { _headingTitleL :: T.Text
                       , _headingTaskTimeL :: TaskTime
                       }
               deriving (Show)

instance Put Heading where
  put (Heading name time) = T.concat [headingPrefix <+ 2, name <+ 2,
                                      put time, headingSuffix, newline 2
                                     ]

makeLenses ''Heading

-- Tags

newtype Tags = Tags { _tagsL :: [T.Text]
                    }
               deriving (Show)

makeLenses ''Tags

instance Put Tags where
  put (Tags (tag:tags)) = T.concat [2 +> tagsPrefix, 2 +> tagsStr, tagsSuffix]
    where
      tagsStr = T.append tag $ foldMap (T.append ", ") tags


newtype Desc = Desc T.Text
               deriving (Show)

instance Put Desc where
  put (Desc str) = T.concat [2 +> descPrefix, 2 +> str, descSuffix, newline 2]


data Task = Task { _taskHeadingL :: Heading
                 , _taskDescL :: Maybe Desc
                 , _taskTagsL :: Tags
                 } deriving (Show)

makeLenses ''Task


instance Put Task where
  put (Task heading Nothing tags)     = T.concat [put heading, put tags,
                                                  newline 2, taskSep
                                                 ]
  put (Task heading (Just desc) tags) = T.concat [put heading, put desc, put tags,
                                                  newline 2, taskSep, newline 2
                                                 ]
