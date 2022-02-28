{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Log ( Log
                     , newLog
                     , insertTask
                     , logDateL
                     , logTasksL
                     ) where

import Relude hiding (put)

import qualified Data.Text as T
import Data.Sequence( Seq
                    , (|>)
                    )

import Data.Time (Day)
import Control.Lens ( makeLenses
                    , (%~)
                    )

import Miku.Data.Task

import Miku.Data.Parser
import Miku.Data.Types
import Miku.Data.Syntax


data Log = Log { _logDateL  :: Text
               , _logTasksL :: Seq Task
               }
              deriving (Show)

makeLenses ''Log
-- Syntax

datePrefix = "## Date:"

instance Element Log where
  prefix               = const $ Just (datePrefix <+ 2)
  sep (Log date _)     = Just  $ newElem date
  suffix (Log _ tasks) = Just  $ foldMap put tasks
  parse                = logP


logP :: Parser Log
logP = do
         spaceP
         symbP datePrefix
         date <- T.strip <$> spanTokenP elemSuffix
         symbP elemSuffix
         Log date . fromList <$> many parse


newLog :: Day -> Log
newLog day = Log (show day) empty

insertTask :: Task -> Log -> Log
insertTask task = logTasksL %~ (|> task)

