{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Miku.Types.Log
  ( Log (Log, logHeading, logTasks),
    LogP,
    DayP,
    DayF,
    Heading(..),
    HeadingP,
    HeadingF,
    readLog,
    writeLog
  )
where

import qualified Data.Text as T
import           Data.Time (Day)
import           Text.Megaparsec (runParser, parseTest)
import           Text.Read (read)

import           Relude

import           Miku.Types.Parser
import           Miku.Types.Time


type DayP = Digits <: Literal "-" :>> Digits <: Literal "-" :>> Digits
type DayF = Integer -> Integer -> Integer -> Day

dayP :: DayF
dayP y m d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

instance Atom DayP where
  type AtomType DayP = Day
  parseAtom    = composeP @DayP dayP
  showAtom day = show day
  -- showAtom 

-----------------------------------------------------------------
-- | 'Heading'
-----------------------------------------------------------------

newtype Heading = Heading {getHeading :: Day}
  deriving (Show)

type HeadingP = Prefix "# Date:" :>> DayP <: Many Space
type HeadingF = () -> Day -> Heading

headingP :: () -> Day -> Heading
headingP _ = Heading

instance Atom HeadingP where
  type AtomType HeadingP = Heading

  parseAtom              = composeP @HeadingP headingP
  showAtom (Heading day) = composeS @HeadingP @HeadingF "" () day

-----------------------------------------------------------------
-- | 'Task'
-----------------------------------------------------------------

data Task = Task
  { taskName  :: Text,
    taskStart :: Time,
    taskEnd   :: Maybe Time,
    taskDesc  :: Maybe Text
  }
  deriving (Show)

type TaskF = Text -> Time -> Maybe Time -> Maybe Text -> Task


type TaskSep = Many Newline :> Literal "---" <: Newline <: Newline <: Newline <: Many Newline
type Desc    = (Literal "  " :> PrintChar <: Newline <: Newline)

type TaskP = (Prefix "###" :> TakeTill "(" <: Token "(")
          :>> TimeP <: Space <: Token "-" <: Space :>> Optional TimeP <: Token ")" <: Newline <: Newline <: Many Newline
          :>> Optional Desc <: TaskSep
          -- <: TaskSep
instance Atom TaskP where
  type AtomType TaskP = Task
  parseAtom = composeP @TaskP (Task . T.strip)
  showAtom (Task name start end desc) = composeS @TaskP @TaskF "" name start end desc

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log = Log
  { logHeading :: Heading,
    logTasks   :: [Task]
  }
  deriving (Show)

type LogP = HeadingP <: Newline <: Newline <: Newline <: Many Newline :>> Many TaskP
type LogF = Heading -> [Task] -> Log

instance Atom LogP where
  type AtomType LogP = Log
  parseAtom = composeP @LogP Log
  showAtom (Log h ts) = composeS @LogP @LogF "" h ts

readLog :: FilePath -> IO Log
readLog f = do
  input <- T.pack <$> readFile f

  case runParser (parseAtom @LogP) "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log

writeLog :: FilePath -> Log -> IO()
writeLog f log = writeFile f $ T.unpack (showAtom @LogP log)

-- writeLog :: FilePath -> IO Log
-- writeLog 

-- readLog :: FilePath -> IO ()
-- readLog f = do
--   input <- T.pack <$> readFile f
--
--   print input
--
--   parseTest (parseAtom @LogP) input
--
