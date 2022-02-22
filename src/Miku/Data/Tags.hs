{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Miku.Data.Tags ( Tags(..)
                      , tagsL
                      , tagsP
                      ) where

import Relude
import qualified Data.Text as T
import qualified Data.Sequence as Seq

import Control.Lens (makeLenses)

import Parser
import Types
import Syntax


pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>)

newtype Tags = Tags { _tagsL :: Seq Text
                    }
               deriving (Show)

-- Syntax For tags

tagsPrefix :: Text
tagsPrefix = "`Tags:"

tagsSuffix :: Text
tagsSuffix = "`"

tagsSep :: Text
tagsSep = ","

--

makeLenses ''Tags

instance Element Tags where
  prefix = const $ Just (2 +> tagsPrefix <+ 2)
  sep (Tags (x:<xs)) = Just $ x <> foldMap (", " <> ) xs
  suffix    = const $ Just (tagsSuffix <> elemSuffix <> newline 2)
  parse = tagsP


tagsP :: Parser Tags
tagsP = do
          symbP tagsPrefix
          tags <- fromList . T.splitOn tagsSep <$> spanTokenP tagsSuffix
          symbP tagsSuffix
          return (Tags $ T.strip <$> tags)


-- import qualified Data.Sequence as Seq

