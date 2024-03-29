{-# OPTIONS_GHC -Wno-orphans #-}
module Miku.Templates.Log
  ( Heading (Heading)
  , Log (Log)
  , Task (..)
  , TaskDesc (TaskDesc)
  , TaskName (TaskName)
  , TaskTag (TaskTag)
  , Todo (Todo)
  , TodoStatus (Done, NotDone)
  , descL
  , headingL
  , logHeadingL
  , logTasksL
  , logTodosL
  , nameL
  , tagL
  , taskDescL
  , taskEndL
  , taskNameL
  , taskStartL
  , taskTagsL
  , todoDescL
  , todoStatusL
    -- * Helper functions
  , logParser
  , ongoingTask
  , showHeading
  , showLog
  , showTags
  , showTask
  , todosDone
  , todosNotDone
    -- * IO Stuff
  , readCurrentLog
  , readLog
  , writeLog
  ) where

import Control.Lens               (_head, filtered, folded, makeLenses, (^.),
                                   (^..), (^?))
import Control.Monad.Trans.Except (throwE)
import Data.Text                  qualified as T
import Data.Time                  (Day)

import Miku.Types.Parser
import Miku.Types.Time            (Time, getCurrentDay)

import System.Directory           (doesFileExist, doesPathExist)
import System.FilePath            ((</>))

import Text.Megaparsec            (runParser)
import Text.Read                  (read)

import Relude


dayP :: DayF
dayP y m d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

type DayFormat = Digits <: Literal "-" :+> Digits <: Literal "-" :+> Digits
type DayF      = Integer -> Integer -> Integer -> Day

instance MkBluePrint Day where
  type Format Day   = DayFormat
  type Function Day = DayF

  parseBP = dayP
  showBP day = show day

-----------------------------------------------------------------
-- | 'Heading'
-----------------------------------------------------------------

newtype Heading
  = Heading { _headingL :: Day }
  deriving stock (Eq, Show)

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

newtype TaskName
  = TaskName { _nameL :: Text }
  deriving stock (Eq, Show)

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

newtype TaskDesc
  = TaskDesc { _descL :: Text }
  deriving stock (Eq, Show)


type DescLine = Repeat 2 Space :> AlphaNums
            :+> PrintChars <: Newline

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

newtype TaskTag
  = TaskTag { _tagL :: Text }
  deriving stock (Eq, Show)

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

data Task
  = Task
      { _taskNameL  :: TaskName
      , _taskStartL :: Time
      , _taskEndL   :: Maybe Time
      , _taskDescL  :: Maybe TaskDesc
      , _taskTagsL  :: [TaskTag]
      }
  deriving stock (Show)

type TaskF   = TaskName -> Time -> Maybe Time -> Maybe TaskDesc -> [TaskTag] -> Task
type TaskSep = Many Newline :> Literal "---" <: Repeat 3 Newline <: Many Newline

type TaskFormat = BluePrint TaskName
              :+> BluePrint Time <: Space <: Token "-" <: Space
              :+> Optional (BluePrint Time) <: Token ")" <: Repeat 2 Newline <: Many Newline
              :+> Optional (Try (BluePrint TaskDesc))
              :+> BluePrint [TaskTag] <: TaskSep

instance MkBluePrint Task where
  type Format Task = TaskFormat
  type Function Task = TaskF

  parseBP                                = Task
  showBP (Task name start end desc tags) = composeS @TaskFormat @TaskF mempty name start end desc tags

makeLenses ''Task

showTask :: Task -> Text
showTask = showAtom @(BluePrint Task)

-----------------------------------------------------------------
-- | 'Todos'
-----------------------------------------------------------------

data TodoStatus
  = Done
  | NotDone
  deriving stock (Eq, Show)

data Todo
  = Todo
      { _todoStatusL :: TodoStatus
      , _todoDescL   :: Text
      }
  deriving stock (Show)

type TodoFormat =  Repeat 2 Space :> Literal "-" :> Space
               :>  Literal "[" :> PrintChar <: Literal "]" <: Space
               :+> PrintChars <: Repeat 2 Newline

type TodoF = Char -> Text -> Todo

todo :: Char -> Text -> Todo
todo ' ' = Todo NotDone
todo 'X' = Todo Done
todo _   = Todo NotDone

instance MkBluePrint Todo where
  type Format Todo   = TodoFormat
  type Function Todo = TodoF

  parseBP = todo
  showBP  (Todo Done desc)    = composeS @TodoFormat @TodoF mempty 'X' desc
  showBP  (Todo NotDone desc) = composeS @TodoFormat @TodoF mempty ' ' desc

makeLenses ''Todo

-----------------------------------------------------------------
-- | 'Log' type stores every information about the current log.
-----------------------------------------------------------------

data Log
  = Log
      { _logHeadingL :: Heading
      , _logTasksL   :: [Task]
      , _logTodosL   :: [Todo]
      }
  deriving stock (Show)

type LogFormat = BluePrint Heading <: Repeat 3 Newline <: Many Newline
             :+> Many (BluePrint Task)
             :+> Prefix "## TODOs:" :> Repeat 2 Newline
              :> Many (BluePrint Todo)

type LogF = Heading -> [Task] -> [Todo] -> Log

instance MkBluePrint Log where
  type Format Log   = LogFormat
  type Function Log = LogF

  parseBP = Log
  showBP (Log heading tasks todos) = composeS @LogFormat @LogF mempty heading tasks todos

makeLenses ''Log

-----------------------------------------------------------------
-- | Helper functions
-----------------------------------------------------------------

showHeading :: Heading -> Text
showHeading = showAtom @(BluePrint Heading)

logParser :: Parser Log
logParser = parseAtom @(BluePrint Log)

showLog :: Log -> Text
showLog = showAtom @(BluePrint Log)

showTags :: [TaskTag] -> Text
showTags = showAtom @(BluePrint [TaskTag])

mkNewLog :: Heading -> Log
mkNewLog date = Log date [] []

ongoingTask :: Log -> Maybe Task
ongoingTask log =
  do
    latestTask <- log ^? logTasksL . _head
    guard (isNothing $ latestTask ^. taskEndL)
    return latestTask

todosNotDone :: [Todo] -> [Todo]
todosNotDone todos = todos ^.. folded . filtered (\g -> g ^. todoStatusL == NotDone)

todosDone :: [Todo] -> [Todo]
todosDone todos = todos ^.. folded . filtered (\g -> g ^. todoStatusL == Done)

getLogName :: Day -> String
getLogName day = show day <> "-log.md"

getLogPath :: FilePath -> Day -> FilePath
getLogPath logsDir day = logsDir </> getLogName day

-----------------------------------------------------------------
-- | IO Stuff
-----------------------------------------------------------------

logsDirExists :: FilePath -> ExceptT Text IO ()
logsDirExists logsDir =
  do
    dirExists <- liftIO $ doesPathExist logsDir

    unless dirExists
      (throwE $ "The following directory for logs doesn't exists: " <> T.pack logsDir)

readLog :: FilePath -> Day -> ExceptT Text IO Log
readLog logsDir day =
  do

    void $ logsDirExists logsDir

    let logPath :: FilePath
        logPath = getLogPath logsDir day

        logName :: String
        logName = getLogName day

    logExists <- liftIO $ doesFileExist logPath

    unless logExists
      (throwE $ "The following log file doesn't exist: " <> T.pack logPath)

    input <- readFileText logPath

    case runParser logParser logPath input of
      Left _    -> throwE $ "Failed to parse log from " <> T.pack logName
      Right log -> return log

readCurrentLog :: FilePath -> ExceptT Text IO Log
readCurrentLog logsDir = do
  currDay <- liftIO getCurrentDay

  let logPath :: FilePath
      logPath = logsDir </> logName

      logName :: String
      logName = getLogName currDay

      newLog :: Log
      newLog = mkNewLog $ Heading currDay

  logExists <- liftIO $ doesFileExist logPath

  if logExists
    then readLog logsDir currDay
    else writeLog logsDir currDay newLog >> return newLog


writeLog :: FilePath -> Day -> Log -> ExceptT Text IO ()
writeLog logsDir day log =
  do
    void $ logsDirExists logsDir

    let logPath :: FilePath
        logPath = logsDir </> logName

        logName :: String
        logName = getLogName day

    writeFileText logPath (showLog log)
