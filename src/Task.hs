module Task ( x
            -- , Task
            ) where

import Time



-- |
-- Name of the task. This is the heading in the markdown with prefix of "###"
--


data Name = Name { name :: String
                 , nameString :: String
                 }

instance Show Name where
  show (Name _ str) = str

newName :: String -> Name
newName = Name <*> (prefix ++)
  where
    prefix = "### "


-- |
-- Datatype to keep track of when the tasks begins and ends.
-- This datatype is surrounded by () with the following '-' character
-- acting as a seprator between starting and ending time.
-- The closing brace is followed by \n\n characters.


data TaskTime = TaskTime { start :: Time
                         , end :: Maybe Time
                         , tasktimeString :: String
                         }

instance Show TaskTime where
  show (TaskTime _ _ str) = str

newTaskTime :: Time -> Maybe Time -> TaskTime
newTaskTime start end = case end of
                          Nothing -> TaskTime start end $ str (show start)
                          (Just t) -> TaskTime start end $ str (show start <> " - " <> show t)
  where
    prefix = "  ("
    suffix = ")\n\n"
    str    = \x -> concat [prefix, x , suffix]



-- |
-- Tags of the task. This is surrounded by `` in the markdown
-- and the following line begins with `Tags:` keyword. Each tag is seperated by ','.
--


data Tags = Tags { tags :: [String]
                 , tagsString :: String
                 }

instance Show Tags where
  show (Tags _ str) = str

newTags :: [String] -> Tags
newTags ts = Tags ts $ concat [prefix, tagsStr ts, suffix]
  where
    prefix = "`Tags: "
    suffix = "`"
    tagsStr = (<>) . head <*> foldMap (", " <>) . tail


-- |
-- This datatype stores the description of the task.
-- The suffix of the description if it exists is \n\n .

newtype Desc = Desc { descString :: Maybe String
                    }

instance Show Desc where
  show (Desc Nothing) = ""
  show (Desc (Just str)) = str

newDesc :: Maybe String -> Desc
newDesc = Desc . fmap (++ suffix)
  where
    suffix = "\n\n"


data Task = Task { taskName :: Name
                 , taskTime :: TaskTime
                 , taskDesc :: Desc
                 , taskTags :: Tags
                 }

instance Show Task where
  show (Task name time desc tags) = concat [show name, show time, show desc, show tags]

newTask :: Name -> TaskTime -> Desc -> Tags -> Task
newTask = Task


-----------------------------------------------

x :: Task
x = Task { taskName = newName "Heading"
         , taskDesc = newDesc (Just "produced output (of type a) to our continuation k, which produces a parser that is executed with the remainder of the output.")
         , taskTags = newTags ["Tag 1", "Tag 2"]
         , taskTime = newTaskTime (newTime 10 2) (Just $ newTime 13 2)
         }
