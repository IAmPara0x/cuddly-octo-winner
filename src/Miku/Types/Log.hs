{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Miku.Types.Log
  ( Log (Log, logHeading, logTasks),
    LogP,
    readLog,
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

dayP :: Integer -> Integer -> Integer -> Day
dayP y m d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

instance Atom DayP where
  type AtomP DayP = Day
  atomP = composeP @DayP dayP


-----------------------------------------------------------------
-- | 'Heading'
-----------------------------------------------------------------

newtype Heading = Heading {getHeading :: Day}
  deriving (Show)

type HeadingP = Prefix "# Date: " :>> DayP <: Many Space

headingP :: () -> Day -> Heading
headingP _ = Heading

instance Atom HeadingP where
  type AtomP HeadingP = Heading
  atomP = composeP @HeadingP headingP

-----------------------------------------------------------------
-- | 'Task'
-----------------------------------------------------------------

data Task = Task
  { taskName :: Text,
    taskStart :: Time,
    taskEnd :: Maybe Time,
    taskDesc :: Maybe Text
  }
  deriving (Show)

type TaskSep = Many Newline :> Literal "---" <: Some Newline
type Desc    = Some (Literal "  " :> PrintChar <: Newline)

type TaskP = (Prefix "###" :> TakeTill "(" <: Token "(")
          :>> TimeP <: Token "-"
          :>> Optional TimeP <: Token ")" <: Many Newline
          :>> Optional Desc <: TaskSep
          -- <: TaskSep

instance Atom TaskP where
  type AtomP TaskP = Task
  atomP = composeP @TaskP (\name startT endT xs -> Task (T.strip name) startT endT $ foldMap (<> "\n") <$> xs)

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log = Log
  { logHeading :: Heading,
    logTasks   :: [Task]
  }
  deriving (Show)

type LogP = HeadingP <: Some Newline :>> Many TaskP

instance Atom LogP where
  type AtomP LogP = Log
  atomP = composeP @LogP Log

readLog :: FilePath -> IO Log
readLog f = do
  input <- T.pack <$> readFile f

  case runParser (atomP @LogP) "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log

-- readLog :: FilePath -> IO ()
-- readLog f = do
--   input <- T.pack <$> readFile f
--
--   print input
--
--   parseTest (atomP @LogP) input
--
