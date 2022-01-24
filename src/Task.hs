module Task ( x
            , Task
            ) where

import Time


newtype Name = Name String

instance Show Name where
  show (Name t) = concat ["### ", t, " "]


newtype Tags = Tags [String]

instance Show Tags where
  show (Tags (t:ts)) = concat ["`", "Tags: ", t ++ foldMap (", " <>) ts, "`"]


data TaskTime = TaskTime { start :: Time
                         , end :: Maybe Time
                         }

instance Show TaskTime where
  show (TaskTime start Nothing) = concat ["(", show start, ")"]
  show (TaskTime start (Just end)) = concat ["(", show start, " - ", show end, ")"]


data Task = Task { taskName :: Name
                 , taskDesc :: Maybe String
                 , taskTags :: Tags
                 , taskTime :: TaskTime
                 }

instance Show Task where
  show (Task name Nothing tags time)     = concat [show name, show time, "\n", show tags]

  show (Task name (Just desc) tags time) = concat [show name, show time, "\n\n", desc, "\n\n", show tags ]


---------------------------------------------

x :: Task
x = Task { taskName = Name "Heading"
         , taskDesc = Just "produced output (of type a) to our continuation k, which produces a parser that is executed with the remainder of the output."
         , taskTags = Tags ["Tag 1", "Tag 2"]
         , taskTime = TaskTime { start = newTime 10 2
                               , end   = Just $ newTime 13 2
                               }
         }

