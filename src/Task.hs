module Task ( Heading(..)
            , TaskTime(..)
            , Desc(..)
            , Tags(..)
            , Task(..)
            ) where

import Data.Maybe ( fromJust
                  , isNothing
                  )
import Time (Time)


elemSep :: String
elemSep = "<br><br>\n"

-- |
-- Name of the task. This is the heading in the markdown with prefix of "###"
--


data Heading = Heading { hName :: String
                       , hTime :: TaskTime
                       }

instance Show Heading where
  show (Heading name time) = concat ["#### Task: ", name, " ",
                                     show time, "\n\n", elemSep
                                    ]


-- |
-- Datatype to keep track of when the tasks begins and ends.
-- This datatype is surrounded by () with the following '-' character
-- acting as a seprator between starting and ending time.
-- The closing brace is followed by \n\n characters.


data TaskTime = TaskTime { startT :: Time
                         , endT :: Maybe Time
                         }

instance Show TaskTime where
  show (TaskTime start end)
    | isNothing end = str (show start)
    | otherwise     = str (show start <> " - " <> show (fromJust end))
    where
      prefix = "  ("
      suffix = ")"
      str    = \x -> concat [prefix, x , suffix]


-- |
-- Tags of the task. This is surrounded by `` in the markdown
-- and the following line begins with `Tags:` keyword. Each tag is seperated by ','.
--


newtype Tags = Tags { tags :: [String] }

instance Show Tags where
  show (Tags (t:ts)) = concat [prefix, tagsStr, suffix]
    where
      prefix = "\t`Tags: "
      suffix = "`"
      tagsStr = t ++ foldMap (", " <>) ts



-- |
-- This datatype stores the description of the task.
-- The suffix of the description if it exists is \n\n .
--

newtype Desc = Desc { desc :: String }

instance Show Desc where
  show (Desc str) = concat ["\tDesc: ", str, "\n\n", elemSep]


-- |
-- DataType that contains all the information about the task.

data Task = Task { taskHeading :: Heading
                 , taskDesc :: Maybe Desc
                 , taskTags :: Tags
                 }



instance Show Task where
  show (Task heading Nothing tags)     = show heading ++ show tags
  show (Task heading (Just desc) tags) = concat [show heading, show desc, show tags]

