{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Miku.Types.Log
  ( Heading(..),
    HeadingFormat,
    Task(..),
    TaskFormat,
    LogFormat,
    Log (Log, logHeading, logTasks),
    readLog,
    writeLog
  )
where

import  Data.Text qualified as T
import  Data.Time              (Day)
import  Text.Megaparsec        (runParser, parseTest)
import  Text.Read              (read)

import  Relude

import  Miku.Types.Parser
import  Miku.Types.Time


-- TODO: Find a way to eliminate types like HeadingFormat and HeadingF.

type DayFormat = Digits <: Literal "-" :>> Digits <: Literal "-" :>> Digits
type DayF = Integer -> Integer -> Integer -> Day

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

-----------------------------------------------------------------
-- | 'Task'
-----------------------------------------------------------------

data Task = Task
  { taskName  :: Text,
    taskStart :: Time,
    taskEnd   :: Maybe Time,
    taskDesc  :: Maybe Text,
    taskTags  :: [Text]
  }
  deriving (Show)

type TaskF = Text -> Time -> Maybe Time -> Maybe Text -> [Text] -> Task

type DescLine = (Repeat 2 Space :> AlphaNum) :>> PrintChar <: Newline

instance Atom DescLine where
  type AtomType DescLine = Text
  parseAtom = composeP @DescLine (<>)
  showAtom  = composeS @DescLine @(Text -> Text -> Text) mempty mempty

type Desc  =  Many DescLine :>> Repeat 1 Newline

instance Atom Desc where
  type AtomType Desc = Text
  parseAtom = composeP @Desc (\a () -> T.unlines a)
  showAtom  t = composeS @Desc @([Text] -> () -> Text) mempty (T.lines t)

type TaskSep = Many Newline :> Literal "---" <: Repeat 3 Newline <: Many Newline

type Tags    = Repeat 2 Space :> Literal "**" :> Prefix "Tags: "
            :> SepBy1 AlphaNum (Token "," <: Space <: Many Space)
            <: Literal "**" <: Repeat 2 Newline

type TaskFormat = (Prefix "###" :> TakeTill "(" <: Token "(")
              :>> TimeFormat <: Space <: Token "-" <: Space
              :>> Optional TimeFormat <: Token ")" <: Repeat 2 Newline <: Many Newline
              :>> Optional (Try Desc) :>> Tags <: TaskSep

instance Atom TaskFormat where
  type AtomType TaskFormat = Task
  parseAtom = composeP @TaskFormat @TaskF (Task . T.strip)
  showAtom (Task name start end desc tags) = composeS @TaskFormat @TaskF mempty name start end desc tags

instance Element Task where
  type ElementFormat Task = TaskFormat

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log = Log
  { logHeading :: Heading,
    logTasks   :: [Task]
  }
  deriving (Show)

type LogFormat = HeadingFormat <: Repeat 3 Newline <: Many Newline :>> Many TaskFormat
type LogF = Heading -> [Task] -> Log

instance Atom LogFormat where
  type AtomType LogFormat = Log
  parseAtom = composeP @LogFormat Log
  showAtom (Log h ts) = composeS @LogFormat @LogF mempty h ts

instance Element Log where
  type ElementFormat Log = LogFormat

readLog :: FilePath -> IO Log
readLog f = do
  input <- T.pack <$> readFile f

  case runParser (parseAtom @LogFormat) "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log

writeLog :: FilePath -> Log -> IO()
writeLog f log = writeFile f $ T.unpack (showAtom @LogFormat log)

-- readLog :: FilePath -> IO ()
-- readLog f = do
--   input <- T.pack <$> readFile f
--
--   print input
--
--   parseTest (parseAtom @LogFormat) input
--
