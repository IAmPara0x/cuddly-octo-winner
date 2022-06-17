{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DerivingStrategies #-}

module Miku.Types.Log
  ( Task(..),
    TaskFormat,
    Log (..),
    LogFormat,
    readLog,
    writeLog
  )
where

import  Data.Text    qualified as T
import  Data.Text.IO qualified as T
import  Data.Coerce            (coerce)
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
-- | 'Name'
-----------------------------------------------------------------

newtype Name = Name Text
               deriving stock (Show, Eq)

type NameFormat = (Prefix "###" :>> TakeTill "(" <: Token "(")
type NameF      = () -> Text -> Name

instance Atom NameFormat where
  type AtomType NameFormat = Name
  parseAtom = composeP @NameFormat @NameF (const $ Name . T.strip)
  showAtom  = composeS @NameFormat @NameF mempty () . coerce
  
  
-----------------------------------------------------------------
-- | 'Description'
-----------------------------------------------------------------

newtype Description = Description Text
                      deriving stock (Show, Eq)


type DescLine = (Repeat 2 Space :> AlphaNum) :>> PrintChar <: Newline

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

-----------------------------------------------------------------
-- | 'Tags'
-----------------------------------------------------------------

newtype Tag = Tag Text
              deriving stock (Show, Eq)

type TagsFormat   = (Repeat 2 Space :> Literal "**" :> Prefix "Tags: ")
                 :>> SepBy1 AlphaNum (Token "," <: Space <: Many Space)
                 <: Literal "**" <: Repeat 2 Newline

type TagF         = () -> [Text] -> [Tag]

instance Atom TagsFormat where
  type AtomType TagsFormat = [Tag]
  parseAtom = composeP @TagsFormat @TagF (const $ map Tag)
  showAtom  = composeS @TagsFormat @TagF  mempty () . coerce

-----------------------------------------------------------------
-- | 'Task'
-----------------------------------------------------------------

data Task = Task
  { taskName  :: Name,
    taskStart :: Time,
    taskEnd   :: Maybe Time,
    taskDesc  :: Maybe Description,
    taskTags  :: [Tag]
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
  parseAtom = composeP @TaskFormat @TaskF Task
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
  input <- T.readFile f

  case runParser parseElement "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log

writeLog :: FilePath -> Log -> IO ()
writeLog f = T.writeFile f . showElement

-- readLog :: FilePath -> IO ()
-- readLog f = do
--   input <- T.pack <$> readFile f
--
--   print input
--
--   parseTest (parseAtom @LogFormat) input
--
