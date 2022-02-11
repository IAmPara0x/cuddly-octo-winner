{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Task ( Task(Task)
                      , newTask
                      , taskHeadingL
                      , taskDescL
                      , taskTagsL
                      , taskP
                      , tasksP
                      )
                      where

import Relude hiding (put)
import qualified Data.Text as T
import Control.Lens
import Data.Maybe ( fromJust
                  , isNothing
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

makeLenses ''Task


instance Put Task where
  put (Task heading Nothing tags)     = T.concat [put heading,
                                                  put tags,
                                                  taskSep,
                                                  taskSep,
                                                  newline 2
                                                 ]

  put (Task heading (Just desc) tags) = T.concat [put heading,
                                                  put desc,
                                                  put tags,
                                                  taskSep,
                                                  newline 2
                                                 ]


newTask :: Text -> TaskTime -> Text -> [Text] -> Task
newTask title taskTime "" tags   = Task { _taskHeadingL = Heading title taskTime
                                        , _taskDescL    = Nothing
                                        , _taskTagsL    = Tags tags
                                        }
newTask title taskTime desc tags = Task { _taskHeadingL = Heading title taskTime
                                        , _taskDescL    = Just $ Desc desc
                                        , _taskTagsL    = Tags tags
                                        }

taskP :: Parser Task
taskP = do
          heading <- elemP headingP
          desc <- (Just <$> elemP descP) `mplus`
                  (Nothing <$ return "")
          tags <- elemP tagsP
          symbP taskSep
          return $ Task heading desc tags

tasksP :: Parser [Task]
tasksP = many taskP
