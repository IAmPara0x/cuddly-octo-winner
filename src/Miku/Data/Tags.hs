{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Tags ( Tags(Tags)
                      , tagsL
                      , tagsP
                      ) where

import Relude
import qualified Data.Text as T

import Control.Lens

import Parser
import Types
import Syntax

newtype Tags = Tags { _tagsL :: [Text]
                    }
               deriving (Show)

-- Syntax For tags

tagsPrefix :: Text
tagsPrefix = "`Tags:"

tagsSuffix :: Text
tagsSuffix = "`"

tagsSep :: Char
tagsSep = ','

--

makeLenses ''Tags

instance Element Tags where
  prefix = const $ Just (2 +> tagsPrefix <+ 2)
  sep (Tags (x:xs)) = Just $ x <> foldMap (", " <> ) xs
  suffix    = const $ Just (tagsSuffix <> elemSuffix <> newline 2)
  parse = tagsP


tagsP :: Parser Tags
tagsP = do
          symbP tagsPrefix
          tags <- T.splitOn (T.singleton tagsSep) <$> spanTokenP tagsSuffix
          symbP tagsSuffix
          return (Tags $ map T.strip tags)

