{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Task ( Task(Task)
                      , newTask
                      , completeTask
                      , taskHeadingL
                      , taskDescL
                      , taskTagsL
                      , taskP
                      )
                      where

import Relude hiding (put)

import qualified Data.Text as T
import Data.Sequence (Seq)

import Control.Lens ( makeLenses
                    , (%~)
                    )

import Types
import Parser

import Syntax

import Miku.Data.Time
import Miku.Data.Heading
import Miku.Data.TaskTime
import Miku.Data.Desc
import Miku.Data.Tags


data Task = Task { _taskHeadingL :: Heading
                 , _taskDescL    :: Maybe Desc
                 , _taskTagsL    :: Tags
                 } deriving (Show)

-- Syntax for Task

taskSep :: Text
taskSep = T.replicate 8 "-"

makeLenses ''Task


instance Element Task where
  prefix (Task heading _ _)       = Just  $ put heading
  sep  (Task _ Nothing tags)      = Just  $ put tags
  sep  (Task _ (Just desc) tags)  = Just  $ put desc <> put tags
  suffix                          = const $ Just (taskSep <> newline 2)
  parse                           = taskP


taskP :: Parser Task
taskP = do
          heading <- elemP parse
          desc <- (Just <$> elemP parse) `mplus`
                  (Nothing <$ return "")
          tags <- elemP parse
          symbP taskSep
          return $ Task heading desc tags

tasksP :: Parser [Task]
tasksP = many taskP

newTask :: Text -> TaskTime -> Text -> Seq Text -> Task
newTask title taskTime "" tags   = Task { _taskHeadingL = Heading title taskTime
                                        , _taskDescL    = Nothing
                                        , _taskTagsL    = Tags tags
                                        }
newTask title taskTime desc tags = Task { _taskHeadingL = Heading title taskTime
                                        , _taskDescL    = Just $ Desc desc
                                        , _taskTagsL    = Tags tags
                                        }

completeTask :: Time -> Task -> Task
completeTask t = taskHeadingL . taskTimeL . timeEndL %~ (<|> Just t)

