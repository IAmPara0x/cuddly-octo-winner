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

import           Miku.Types.Heading (Heading)
import           Miku.Types.Task    (Task,taskSep)

data Log = Log { logHeading :: Heading
               , logTasks   :: [Task]
               }

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

readLog :: IO Log
readLog = do
  input <- T.pack <$> readFile "dailyLog.md"

  case runParser logP "dailyLog.md" input of
    Left a    -> error $ T.pack $ show a
    Right log -> return log


writeLog :: FilePath -> Log -> IO ()
writeLog f = T.writeFile f . putElement
