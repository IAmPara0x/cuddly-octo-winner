module Types ( Heading(..)
             , TaskTime(..)
             , Desc(..)
             , Tags(..)
             , Task(..)
             , headingPrefix
             , headingSuffix
             , taskTimePrefix
             , taskTimeSuffix
             , taskTimeSep
             , tagsPrefix
             , tagsSuffix
             , tagsSep
             , descPrefix
             , descSuffix
             , taskSep
             ) where

import Data.Maybe ( fromJust
                  , isNothing
                  )
import Time (Time)


whitespace :: Int -> String
whitespace = flip replicate ' '

newline :: Int -> String
newline = flip replicate '\n'

headingPrefix :: String
headingPrefix = "#### Task: "

headingSuffix :: String
headingSuffix = "<br>"

taskTimePrefix :: String
taskTimePrefix  = "("

taskTimeSuffix :: String
taskTimeSuffix  = ")"

taskTimeSep :: String
taskTimeSep = "-"

tagsPrefix :: String
tagsPrefix = whitespace 2 ++ "`Tags: "

tagsSuffix :: String
tagsSuffix = "`<br>"

tagsSep :: String
tagsSep = ","

descPrefix :: String
descPrefix = whitespace 2 ++ "Desc: "

descSuffix :: String
descSuffix = "<br>"

taskSep :: String
taskSep = replicate 8 '-'


data Heading = Heading { hName :: String
                       , hTime :: TaskTime
                       }

instance Show Heading where
  show (Heading name time) = concat [headingPrefix, name, whitespace 4,
                                     show time, headingSuffix, newline 2]



data TaskTime = TaskTime { startT :: Time
                         , endT :: Maybe Time
                         }

instance Show TaskTime where
  show (TaskTime start end)
    | isNothing end = concat [taskTimePrefix, show start, whitespace 2,
                              taskTimeSep, whitespace 2, taskTimeSuffix
                             ]
    | otherwise     = concat [taskTimePrefix, show start, whitespace 2, taskTimeSep,
                              whitespace 2, show (fromJust end), taskTimeSuffix
                             ]


newtype Tags = Tags { tags :: [String] }

instance Show Tags where
  show (Tags tags) = concat [tagsPrefix, tagsStr, tagsSuffix, newline 2]
    where
      tagsStr = foldMap (++ (tagsSep <> whitespace 1)) tags


newtype Desc = Desc { desc :: String }

instance Show Desc where
  show (Desc str) = concat [descPrefix, str, descSuffix, newline 2]


data Task = Task { taskHeading :: Heading
                 , taskDesc :: Maybe Desc
                 , taskTags :: Tags
                 }


instance Show Task where
  show (Task heading Nothing tags)     = show heading ++ show tags
  show (Task heading (Just desc) tags) = concat [show heading, show desc, show tags]

