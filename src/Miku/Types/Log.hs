{-# OPTIONS_GHC -Wno-orphans    #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Miku.Types.Log
  ( Description(..)
  , Heading(..)
  , Goal(..)
  , Log (..)
  , Name(..)
  , Tag
  , Task(..)
  , Time
  , time
  , readLog
  , writeLog
  )
where

import  Data.Text    qualified as T
import  Data.Text.IO qualified as T
import  Data.Time              (Day)
import  Text.Megaparsec        (runParser, parseTest)
import  Text.Read              (read)

import  Relude

import  Miku.Types.Parser
import  Miku.Types.Time


-- TODO: Find a way to eliminate types like HeadingFormat and HeadingF.

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

newtype Heading = Heading {getHeading :: Day}
  deriving (Show)

type HeadingFormat = Prefix "# Date:" :> BluePrint Day <: Many Space
type HeadingF      = Day -> Heading

instance MkBluePrint Heading where
  type Format Heading = HeadingFormat
  type Function Heading = HeadingF

  parseBP              = Heading
  showBP (Heading day) = composeS @(Format Heading) @(Function Heading) mempty day

instance Element Heading where
  type ElementFormat Heading = HeadingFormat

-----------------------------------------------------------------
-- | 'Name'
-----------------------------------------------------------------

newtype Name = Name Text
               deriving stock (Show, Eq)

type NameFormat = Prefix "###" :> TakeTill "(" <: Token "("
type NameF      = Text -> Name

instance MkBluePrint Name where
  type Format Name   = NameFormat
  type Function Name = NameF

  parseBP = Name
  showBP  = composeS @(Format Name) @(Function Name) mempty . coerce

instance Element Name where
  type ElementFormat Name = NameFormat

-----------------------------------------------------------------
-- | 'Description'
-----------------------------------------------------------------

newtype Description = Description Text
                      deriving stock (Show, Eq)


type DescLine = Repeat 2 Space :> AlphaNums
            :>> PrintChars <: Newline

instance Atom DescLine where
  type AtomType DescLine = Text
  parseAtom = composeP @DescLine (<>)
  showAtom  = composeS @DescLine @(Text -> Text -> Text) mempty mempty

type DescriptionFormat  =  Many DescLine <: Repeat 1 Newline
type DescriptionF       =  [Text] -> Description


instance MkBluePrint Description where
  type Format Description   = DescriptionFormat
  type Function Description = DescriptionF

  parseBP                    = Description . T.unlines
  showBP  (Description desc) = composeS @DescriptionFormat @DescriptionF mempty (T.lines desc)

-----------------------------------------------------------------
-- | 'Tags'
-----------------------------------------------------------------

newtype Tag = Tag Text
              deriving stock (Show, Eq)

type TagsFormat  =  Repeat 2 Space :> Literal "**" :> Prefix "Tags: "
                :>> SepBy1 AlphaNums (Token "," <: Space <: Many Space)
                 <: Literal "**" <: Repeat 2 Newline

type TagF         = () -> [Text] -> [Tag]

instance Atom TagsFormat where
  type AtomType TagsFormat = [Tag]
  parseAtom = composeP @TagsFormat @TagF (const $ map Tag)
  showAtom  = composeS @TagsFormat @TagF  mempty () . coerce

instance Element Tag where
  type ElementFormat Tag = TagsFormat

-----------------------------------------------------------------
-- | 'Task'
-----------------------------------------------------------------

data Task = Task
  { taskName  :: Name
  , taskStart :: Time
  , taskEnd   :: Maybe Time
  , taskDesc  :: Maybe Description
  , taskTags  :: [Tag]
  }
  deriving (Show)

type TaskF = Name -> Time -> Maybe Time -> Maybe Description -> [Tag] -> Task

type TaskSep = Many Newline :> Literal "---" <: Repeat 3 Newline <: Many Newline

type TaskFormat = BluePrint Name
              :>> BluePrint Time <: Space <: Token "-" <: Space
              :>> Optional (BluePrint Time) <: Token ")" <: Repeat 2 Newline <: Many Newline
              :>> Optional (Try (BluePrint Description))
              :>> TagsFormat <: TaskSep

instance MkBluePrint Task where
  type Format Task = TaskFormat
  type Function Task = TaskF

  parseBP                                = Task
  showBP (Task name start end desc tags) = composeS @TaskFormat @TaskF mempty name start end desc tags

-----------------------------------------------------------------
-- | 'Goals'
-----------------------------------------------------------------

data GoalStatus = Done
                | NotDone
                | Other Char
                  deriving stock (Show)

data Goal = Goal
  { goalStatus :: GoalStatus
  , goalDesc   :: Text
  } deriving stock (Show)

type GoalFormat =  Repeat 2 Space :> Literal "-" :> Space
               :>  Literal "[" :> PrintChar <: Literal "]" <: Space
               :>> PrintChars <: Repeat 2 Newline

type GoalF = Char -> Text -> Goal

goal :: Char -> Text -> Goal
goal ' ' = Goal NotDone
goal 'X' = Goal Done
goal c   = Goal (Other c)

instance MkBluePrint Goal where
  type Format Goal   = GoalFormat
  type Function Goal = GoalF

  parseBP = goal
  showBP  (Goal Done desc)      = composeS @GoalFormat @GoalF mempty 'X' desc
  showBP  (Goal NotDone desc)   = composeS @GoalFormat @GoalF mempty ' ' desc
  showBP  (Goal (Other c) desc) = composeS @GoalFormat @GoalF mempty c desc

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log = Log
  { logHeading :: Heading
  , logTasks   :: [Task]
  , logGoals   :: [Goal]
  }
  deriving (Show)

type LogFormat = BluePrint Heading <: Repeat 3 Newline <: Many Newline
             :>> Many (BluePrint Task)
             :>> (Prefix "## Goals:" :> Repeat 2 Newline :> Many (BluePrint Goal))

type LogF = Heading -> [Task] -> [Goal] -> Log

instance MkBluePrint Log where
  type Format Log   = LogFormat
  type Function Log = LogF

  parseBP = Log
  showBP (Log heading tasks goals) = composeS @LogFormat @LogF mempty heading tasks goals


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
