{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}

module Miku.Data.Config ( Config(..)
                        , LogPath(..)
                        , logFPathL
                        , logDirL
                        ) where

import Relude

import qualified Data.Yaml as Y

import Data.Yaml ( FromJSON(..)
                 , ToJSON(..)
                 , (.:)
                 , (.=)
                 )
import Control.Lens ( makeLenses)


newtype LogPath = LogPath FilePath
                  deriving (Show, Eq, Ord,
                            FromJSON, ToJSON,
                            Monoid, Semigroup,
                            ToString
                           )


data Config = Config { _logFPathL  :: Maybe LogPath
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
