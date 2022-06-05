module Miku.Types.Log ( Log(Log)
                      , logHeading
                      , logTasks
                      , readLog
                      , writeLog
                      ) where

import           Text.Show(Show(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Relude hiding (show)

import           Miku.Types.Parser

import           Miku.Types.Heading
import           Miku.Types.Task

data Log = Log { logHeading :: Heading
               , logTasks   :: [Task]
               }

-- type LogP = HeadingP :>

instance Show Log where
  show = T.unpack . putElement

instance Element Log where
  parseElement            = logP
  putElement   (Log h ts) = putElement h <> foldMap (\t -> putElement t <> "\n\n---\n\n") ts

logP :: Parser Log
logP = do
  heading <- parseElement
  void (many eol)
  tasks   <- endBy parseElement taskSep
  void eof
  return $ Log heading tasks

readLog :: FilePath -> IO Log
readLog f = do
  input <- T.pack <$> readFile f

  case runParser logP "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log


writeLog :: FilePath -> Log -> IO ()
writeLog f = T.writeFile f . putElement
