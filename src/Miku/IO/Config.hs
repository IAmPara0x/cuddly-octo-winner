{-#Language RankNTypes#-}
module Miku.IO.Config ( logDir
                      , logPath
                      , writeLogPath
                      , newLogPath
                      ) where

import Relude

import Control.Lens ( (^.)
                    , (&)
                    , (?~)
                    )
import Control.Monad.Trans.Except

import Data.Yaml ( decodeFileEither
                 , encodeFile
                 , ParseException
                 )

import Miku.Data.Config
import Miku.IO.Utils
import Miku.IO.Types

-- Config Commands
--

configPath :: FilePath
configPath = "/home/iamparadox/.miku/config.yml"

readConfig :: EitherIO Config
readConfig = do
               res <- lift $ decodeFileEither configPath
               case res of
                 (Left e)  -> throwE $ msg Err "was not able to read config."
                 (Right v) -> return v

writeConfig :: Config -> EitherIO Config
writeConfig config = lift (encodeFile configPath config)
                     >> return config

instance CmdL Config where
  readM         = createM readConfig (msg Suc "config has been read.")
  writeM config = createM (writeConfig config) (msg Suc "read the config file.")
  newM          = error "`newM` is not implemented for `Config`."


-- File Commands

logPath :: EitherIO FilePath
logPath = do
            config <- readL
            mCall (config ^. logFPathL) return (throwE $ msg Err "was not able to read the file path.")

newLogPath :: EitherIO FilePath
newLogPath = do
               dir  <- logDir
               date <- lift currDate
               return (dir <> show date <> "_log.md")

writeLogPath :: FilePath -> EitherIO (Msg FilePath)
writeLogPath path = readL >>= writeL . (logFPathL ?~ path)
                    >> return (msg Suc $ "updated the file path to the following: " <> path)

logDir :: EitherIO FilePath
logDir = (^. logDirL) <$> readConfig
