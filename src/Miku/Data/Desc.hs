{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Desc ( Desc(Desc)
                      , descL
                      , descP
                      ) where

import Relude
import qualified Data.Text as T
import Control.Lens

import Types
import Parser
import Syntax

newtype Desc = Desc { _descL :: Text
                    } deriving (Show)

makeLenses ''Desc

-- Syntax for Desc
descPrefix :: Text
descPrefix = "Desc:"

instance Put Desc where
  put (Desc str) = newElem $ T.concat [2 +> descPrefix, 2 +> str]

descP :: Parser Desc
descP = do
          symbP descPrefix
          descStr <- spanTokenP elemSuffix
          return (Desc $ T.strip descStr)

