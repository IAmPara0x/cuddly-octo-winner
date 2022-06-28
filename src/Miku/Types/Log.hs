{-# OPTIONS_GHC -Wno-orphans    #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Miku.Types.Log
  ( Goal(Goal)
  , goalStatusL
  , goalDescL
  , GoalStatus(..)

  , Heading(Heading)
  , headingL

  , Log (Log)
  , logHeadingL
  , logTasksL
  , logGoalsL

  , Task(Task)
  , taskNameL
  , taskStartL
  , taskDescL
  , taskEndL
  , taskTagsL

  , TaskDesc(TaskDesc)
  , descL

  , TaskName(TaskName)
  , nameL

  , TaskTag(TaskTag)
  , tagL

  , Time
  , mkTime
  , timeHrsL
  , timeMinsL
  , getCurrentTime

  -- * Helper functions
  , goalsDone
  , goalsNotDone
  , ongoingTask

  -- * IO Stuff
  , readLog
  , writeLog
  )
where

import  Data.Text    qualified as T
import  Data.Text.IO qualified as T
import  Control.Lens
  ( (^?)
  , (^.)
  , ix
  , makeLenses
  )
import  Data.Time              (Day)
import  Text.Megaparsec        (runParser)
import  Text.Read              (read)

import  Relude

import  Miku.Types.Parser
import  Miku.Types.Time


dayP :: DayF
dayP y m d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

type DayFormat = Digits <: Literal "-" :>> Digits <: Literal "-" :>> Digits
type DayF      = Integer -> Integer -> Integer -> Day

instance MkBluePrint Day where
  type Format Day   = DayFormat
  type Function Day = DayF

  parseBP = dayP
  showBP day = show day

-----------------------------------------------------------------
-- | 'Heading'
-----------------------------------------------------------------

newtype Heading = Heading { _headingL :: Day }
                  deriving stock (Show, Eq)

type HeadingFormat = Prefix "# Date:" :> BluePrint Day <: Many Space
type HeadingF      = Day -> Heading

instance MkBluePrint Heading where
  type Format Heading = HeadingFormat
  type Function Heading = HeadingF

  parseBP              = Heading
  showBP (Heading day) = composeS @(Format Heading) @(Function Heading) mempty day

makeLenses ''Heading

-----------------------------------------------------------------
-- | 'TaskName'
-----------------------------------------------------------------

newtype TaskName = TaskName { _nameL :: Text }
               deriving stock (Show, Eq)

type TaskNameFormat = Prefix "###" :> TakeTill "(" <: Token "("
type TaskNameF      = Text -> TaskName

instance MkBluePrint TaskName where
  type Format TaskName   = TaskNameFormat
  type Function TaskName = TaskNameF

  parseBP = TaskName
  showBP  = composeS @(Format TaskName) @(Function TaskName) mempty . coerce

makeLenses ''TaskName

-----------------------------------------------------------------
-- | 'TaskDesc'
-----------------------------------------------------------------

newtype TaskDesc = TaskDesc { _descL :: Text }
                      deriving stock (Show, Eq)


type DescLine = Repeat 2 Space :> AlphaNums
            :>> PrintChars <: Newline

instance Atom DescLine where
  type AtomType DescLine = Text
  parseAtom = composeP @DescLine (<>)
  showAtom  = composeS @DescLine @(Text -> Text -> Text) mempty mempty

type TaskDescFormat  =  Many DescLine <: Repeat 1 Newline
type TaskDescF       =  [Text] -> TaskDesc


instance MkBluePrint TaskDesc where
  type Format TaskDesc   = TaskDescFormat
  type Function TaskDesc = TaskDescF

  parseBP                    = TaskDesc . T.unlines
  showBP  (TaskDesc desc) = composeS @TaskDescFormat @TaskDescF mempty (T.lines desc)

makeLenses ''TaskDesc

-----------------------------------------------------------------
-- | 'TaskTags'
-----------------------------------------------------------------

newtype TaskTag = TaskTag { _tagL :: Text}
              deriving stock (Show, Eq)

type TaskTagsFormat  =  Repeat 2 Space :> Literal "**" :> Prefix "Tags: "
                     :> SepBy1 AlphaNums (Token "," <: Space <: Many Space)
                     <: Literal "**" <: Repeat 2 Newline
type TaskTagsF       = [Text] -> [TaskTag]

instance MkBluePrint [TaskTag] where
  type Format [TaskTag] = TaskTagsFormat
  type Function [TaskTag] = TaskTagsF

  parseBP = map TaskTag
  showBP  = composeS @TaskTagsFormat @TaskTagsF mempty . coerce

makeLenses ''TaskTag

-----------------------------------------------------------------
-- | 'Task'
-----------------------------------------------------------------

data Task = Task
  { _taskNameL  :: TaskName
  , _taskStartL :: Time
  , _taskEndL   :: Maybe Time
  , _taskDescL  :: Maybe TaskDesc
  , _taskTagsL  :: [TaskTag]
  }
  deriving (Show)

type TaskF   = TaskName -> Time -> Maybe Time -> Maybe TaskDesc -> [TaskTag] -> Task
type TaskSep = Many Newline :> Literal "---" <: Repeat 3 Newline <: Many Newline

type TaskFormat = BluePrint TaskName
              :>> BluePrint Time <: Space <: Token "-" <: Space
              :>> Optional (BluePrint Time) <: Token ")" <: Repeat 2 Newline <: Many Newline
              :>> Optional (Try (BluePrint TaskDesc))
              :>> BluePrint [TaskTag] <: TaskSep

instance MkBluePrint Task where
  type Format Task = TaskFormat
  type Function Task = TaskF

  parseBP                                = Task
  showBP (Task name start end desc tags) = composeS @TaskFormat @TaskF mempty name start end desc tags

makeLenses ''Task

-----------------------------------------------------------------
-- | 'Goals'
-----------------------------------------------------------------

data GoalStatus = Done
                | NotDone
                  deriving stock (Show, Eq)

data Goal = Goal
  { _goalStatusL :: !GoalStatus
  , _goalDescL   :: !Text
  } deriving stock (Show)

type GoalFormat =  Repeat 2 Space :> Literal "-" :> Space
               :>  Literal "[" :> PrintChar <: Literal "]" <: Space
               :>> PrintChars <: Repeat 2 Newline

type GoalF = Char -> Text -> Goal

goal :: Char -> Text -> Goal
goal ' ' = Goal NotDone
goal 'X' = Goal Done
goal _   = Goal NotDone

instance MkBluePrint Goal where
  type Format Goal   = GoalFormat
  type Function Goal = GoalF

  parseBP = goal
  showBP  (Goal Done desc)      = composeS @GoalFormat @GoalF mempty 'X' desc
  showBP  (Goal NotDone desc)   = composeS @GoalFormat @GoalF mempty ' ' desc

makeLenses ''Goal

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log = Log
  { _logHeadingL :: !Heading
  , _logTasksL   :: ![Task]
  , _logGoalsL   :: ![Goal]
  }
  deriving (Show)

type LogFormat = BluePrint Heading <: Repeat 3 Newline <: Many Newline
             :>> Many (BluePrint Task)
             :>> Prefix "## Goals:" :> Repeat 2 Newline
              :> Many (BluePrint Goal)

type LogF = Heading -> [Task] -> [Goal] -> Log

instance MkBluePrint Log where
  type Format Log   = LogFormat
  type Function Log = LogF

  parseBP = Log
  showBP (Log heading tasks goals) = composeS @LogFormat @LogF mempty heading tasks goals


makeLenses ''Log

-----------------------------------------------------------------
-- | Helper functions
-----------------------------------------------------------------

ongoingTask :: Log -> Maybe Task
ongoingTask log = log ^. logTasksL ^? ix 0
              >>= \t -> if isNothing (t ^. taskEndL) then return t else Nothing

goalsNotDone :: [Goal] -> [Goal]
goalsNotDone = filter (\g -> (g ^. goalStatusL) == NotDone)

goalsDone :: [Goal] -> [Goal]
goalsDone = filter (\g -> (g ^. goalStatusL) == Done)

-----------------------------------------------------------------
-- | IO Stuff
-----------------------------------------------------------------

readLog :: FilePath -> IO Log
readLog f = do
  input <- T.readFile f
  -- parseTest (parseAtom @LogFormat) input

  case runParser (parseAtom @(BluePrint Log)) "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log

writeLog :: FilePath -> Log -> IO ()
writeLog f = T.writeFile f . showAtom @(BluePrint Log)
