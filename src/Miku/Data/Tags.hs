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

instance Put Tags where
  put (Tags (tag:tags)) = newElem $ surroundElem (2 +> tagsPrefix) tagsSuffix (2 +> tagsStr)
    where
      tagsStr = T.append tag $ foldMap (T.append ", ") tags


tagsP :: Parser Tags
tagsP = do
          symbP tagsPrefix
          tags <- T.splitOn (T.singleton tagsSep) <$> spanTokenP tagsSuffix
          symbP tagsSuffix
          return (Tags $ map T.strip tags)

