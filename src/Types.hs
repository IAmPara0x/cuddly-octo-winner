{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types ( Time
             , newTime
             , Heading(Heading)
             , headingTitleL
             , headingTaskTimeL
             , TaskTime(TaskTime)
             , tagsL
             , taskTimeStartL
             , taskTimeEndL
             , Desc(Desc)
             , Tags(Tags)
             , Task(Task)
             , newTask
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
    | isNothing end = elem $ T.concat [put start <+ 2,
                                       T.singleton taskTimeSep <+ 2
                                      ]
    | otherwise     = elem $ T.concat [put start <+ 2,
                                       T.singleton taskTimeSep <+ 2,
                                       put (fromJust end)
                                      ]
    where
      elem = surroundElem (T.singleton taskTimePrefix) (T.singleton taskTimeSuffix)

-- Heading

data Heading = Heading { _headingTitleL :: T.Text
                       , _headingTaskTimeL :: TaskTime
                       }
               deriving (Show)

instance Put Heading where
  put (Heading name time) = newElem $ T.concat [headingPrefix <+ 2,
                                                name <+ 2, put time
                                               ]


makeLenses ''Heading

-- Tags

newtype Tags = Tags { _tagsL :: [T.Text]
                    }
               deriving (Show)

makeLenses ''Tags

instance Put Tags where
  put (Tags (tag:tags)) = newElem $ surroundElem (2 +> tagsPrefix) tagsSuffix (2 +> tagsStr)
    where
      tagsStr = T.append tag $ foldMap (T.append ", ") tags


newtype Desc = Desc T.Text
               deriving (Show)

instance Put Desc where
  put (Desc str) = newElem $ T.concat [2 +> descPrefix, 2 +> str]


data Task = Task { _taskHeadingL :: Heading
                 , _taskDescL :: Maybe Desc
                 , _taskTagsL :: Tags
                 } deriving (Show)

makeLenses ''Task


instance Put Task where
  put (Task heading Nothing tags)     = T.concat [put heading, put tags, taskSep, taskSep, newline 2]
  put (Task heading (Just desc) tags) = T.concat [put heading, put desc, put tags,
                                                  taskSep, newline 2
                                                 ]


newTask :: T.Text -> TaskTime -> T.Text -> [T.Text] -> Task
newTask title taskTime "" tags   = Task { _taskHeadingL = Heading title taskTime
                                        , _taskDescL    = Nothing
                                        , _taskTagsL    = Tags tags
                                        }
newTask title taskTime desc tags = Task { _taskHeadingL = Heading title taskTime
                                        , _taskDescL    = Just $ Desc desc
                                        , _taskTagsL    = Tags tags
                                        }
