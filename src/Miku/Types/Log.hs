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

type DayFormat = Digits <: Literal "-" :>> Digits <: Literal "-" :>> Digits
type DayF      = Integer -> Integer -> Integer -> Day

dayP :: DayF
dayP y m d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

instance Atom DayFormat where
  type AtomType DayFormat = Day
  parseAtom    = composeP @DayFormat dayP
  showAtom day = show day
  -- showAtom

-----------------------------------------------------------------
-- | 'Heading'
-----------------------------------------------------------------

newtype Heading = Heading {getHeading :: Day}
  deriving (Show)

type HeadingFormat = Prefix "# Date:" :>> DayFormat <: Many Space
type HeadingF = () -> Day -> Heading

headingP :: () -> Day -> Heading
headingP _ = Heading

instance Atom HeadingFormat where
  type AtomType HeadingFormat = Heading

  parseAtom              = composeP @HeadingFormat headingP
  showAtom (Heading day) = composeS @HeadingFormat @HeadingF mempty () day

instance Element Heading where
  type ElementFormat Heading = HeadingFormat

-----------------------------------------------------------------
-- | 'Name'
-----------------------------------------------------------------

newtype Name = Name Text
               deriving stock (Show, Eq)

type NameFormat = Prefix "###" :>> TakeTill "(" <: Token "("
type NameF      = () -> Text -> Name

instance Atom NameFormat where
  type AtomType NameFormat = Name
  parseAtom = composeP @NameFormat @NameF (const $ Name . T.strip)
  showAtom  = composeS @NameFormat @NameF mempty () . coerce
  
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

type DescriptionFormat  =  Many DescLine :>> Repeat 1 Newline
type DescriptionF       =  [Text] -> () -> Description

instance Atom DescriptionFormat where
  type AtomType DescriptionFormat = Description
  parseAtom = composeP @DescriptionFormat @DescriptionF (\a () -> Description $ T.unlines a)
  showAtom  = composeS @DescriptionFormat @DescriptionF mempty . T.lines . coerce

instance Element Description where
  type ElementFormat Description = DescriptionFormat

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

type TaskFormat = NameFormat
              :>> TimeFormat <: Space <: Token "-" <: Space
              :>> Optional TimeFormat <: Token ")" <: Repeat 2 Newline <: Many Newline
              :>> Optional (Try DescriptionFormat)
              :>> TagsFormat <: TaskSep

instance Atom TaskFormat where
  type AtomType TaskFormat = Task
  parseAtom                                = composeP @TaskFormat @TaskF Task
  showAtom (Task name start end desc tags) = composeS @TaskFormat @TaskF mempty name start end desc tags

instance Element Task where
  type ElementFormat Task = TaskFormat

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

instance Atom GoalFormat where
  type AtomType GoalFormat = Goal
  parseAtom                       = composeP @GoalFormat @GoalF goal
  showAtom  (Goal Done desc)      = composeS @GoalFormat @GoalF mempty 'X' desc
  showAtom  (Goal NotDone desc)   = composeS @GoalFormat @GoalF mempty ' ' desc
  showAtom  (Goal (Other c) desc) = composeS @GoalFormat @GoalF mempty c desc

instance Element Goal where
  type ElementFormat Goal = GoalFormat

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log = Log
  { logHeading :: Heading
  , logTasks   :: [Task]
  , logGoals   :: [Goal]
  }
  deriving (Show)

type LogFormat = HeadingFormat <: Repeat 3 Newline <: Many Newline
             :>> Many TaskFormat
             :>> (Prefix "## Goals:" :> Repeat 2 Newline :> Many GoalFormat)
             
type LogF = Heading -> [Task] -> [Goal] -> Log

instance Atom LogFormat where
  type AtomType LogFormat = Log
  parseAtom                          = composeP @LogFormat Log
  showAtom (Log heading tasks goals) = composeS @LogFormat @LogF mempty heading tasks goals

instance Element Log where
  type ElementFormat Log = LogFormat

-----------------------------------------------------------------
-- | IO Stuff
-----------------------------------------------------------------

readLog :: FilePath -> IO Log
readLog f = do
  input <- T.readFile f
  -- parseTest (parseAtom @LogFormat) input
  
  case runParser parseElement "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log

writeLog :: FilePath -> Log -> IO ()
writeLog f = T.writeFile f . showElement
