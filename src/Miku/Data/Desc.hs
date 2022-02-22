{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Desc ( Desc(Desc)
                      , descL
                      , descP
                      ) where

import Relude
import qualified Data.Text as T
import Control.Lens (makeLenses)

import Types
import Parser
import Syntax

newtype Desc = Desc { _descL :: Text
                    } deriving (Show)

makeLenses ''Desc

-- Syntax for Desc
descPrefix :: Text
descPrefix = "Desc:"

instance Element Desc where
  prefix         = const $ Just (2 +> "Desc:" <+ 2)
  sep (Desc str) = Just str
  suffix         = const $ Just (elemSuffix <> newline 2)
  parse          = descP

descP :: Parser Desc
descP = do
          symbP descPrefix
          descStr <- spanTokenP elemSuffix
          return (Desc $ T.strip descStr)
