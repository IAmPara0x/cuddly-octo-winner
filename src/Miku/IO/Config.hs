{-# LANGUAGE TypeSynonymInstances #-}

module Miku.IO.Config ( logDir
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
  prefixMsg Err    = "Config Error:"
  prefixMsg Suc    = "Config Success:"
  
  readM            = createM readConfig
                   $ msg Suc "config has been read."

  writeM config    = createM (writeConfig config)
                   $ msg Suc "read the config file."

  newM             = createM (throwE $ msg Err "`newM` is not implemented for `Config`.") Err


-- File Commands

readLogPath :: EitherIO LogPath
readLogPath = do
                config <- readL
                mCall (config ^. logFPathL) return (throwE $ msg Err "was not able to read the file path.")

newLogPath :: EitherIO LogPath
newLogPath = do
               dir  <- logDir
               date <- lift currDate
               return $ LogPath (dir <> show date <> "_log.md")

writeLogPath :: LogPath -> EitherIO LogPath
writeLogPath path = readL >>= writeL . (logFPathL ?~ path)
                    >> return path

instance CmdL LogPath where
  prefixMsg  Err = "LogPath Error: "
  prefixMsg  Suc = "LogPath Success: "

  readM          = createM readLogPath
                 $ msg Suc "read the current log path."

  writeM path    = createM (writeLogPath path)
                 $ msg Suc $ "log path has been updated to: " <> toString path

  newM           = createM (newLogPath >>= writeL)
                 $ msg Suc "new log path ."


logDir :: EitherIO FilePath
logDir = (^. logDirL) <$> readConfig
