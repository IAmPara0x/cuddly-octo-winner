{-# LANGUAGE TemplateHaskell #-}
module Miku.Data.Config ( Config(..)
                        , filePathL
                        , logDirL
                        ) where

import Relude

import qualified Data.Yaml as Y

import Data.Yaml ( FromJSON(..)
                 , ToJSON(..)
                 , (.:)
                 , (.=)
                 )
import Control.Lens (makeLenses)

data Config = Config { _filePathL  :: Maybe FilePath
                     , _logDirL    :: FilePath
                     } deriving (Eq, Show)

makeLenses ''Config

instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$>
                           v .: "filepath" <*>
                           v .: "log-dir"

  parseJSON _            = fail "Expected Object for Config value."

instance ToJSON Config where
  toJSON (Config f dir) = Y.object ["filepath" .= f, "log-dir" .= dir]
