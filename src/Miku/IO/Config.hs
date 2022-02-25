module Miku.IO.Config ( readConfig
                      , writeConfig
                      , filePath
                      , newFilePath
                      , logDir
                      ) where

import Relude

import Control.Lens ( (^.)
                    )
import Control.Monad.Trans.Except

import Data.Yaml ( decodeFileEither
                 , encodeFile
                 )

import Miku.Data.Config

import Miku.IO.Utils

configPath :: FilePath
configPath = "/home/iamparadox/.miku/config.yml"

readConfig :: EitherIO Config
readConfig = do
               res <- lift $ decodeFileEither configPath
               case res of
                 (Left e)  -> throwE $ show e
                 (Right v) -> return v

writeConfig :: Config -> EitherIO String
writeConfig config = lift (encodeFile configPath config)
                  >> return "config file has been written."

filePath :: EitherIO FilePath
filePath = do
             config <- readConfig
             mCall (config ^. filePathL) return (throwE "Log file was not created.")

newFilePath :: EitherIO FilePath
newFilePath = do
                dir  <- logDir
                date <- lift currDate
                return (dir <> show date <> "_log.md")
                
logDir :: EitherIO FilePath
logDir = (^. logDirL) <$> readConfig
