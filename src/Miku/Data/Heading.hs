{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Heading ( Heading(Heading)
                         , titleL
                         , taskTimeL
                         , headingP
                         )
                         where
import Relude hiding (put)
import Control.Lens
import qualified Data.Text as T

import Parser
import Types
import Syntax

import Miku.Data.TaskTime

data Heading = Heading { _titleL :: Text
                       , _taskTimeL :: TaskTime
                       }
               deriving (Show)

-- Syntax for heading
headingPrefix :: Text
headingPrefix = "#### Task:"
--

makeLenses ''Heading

instance Put Heading where
  put (Heading name time) = newElem $ T.concat [headingPrefix <+ 2,
                                                name <+ 2, put time
                                               ]
headingP :: Parser Heading
headingP = do
             symbP headingPrefix
             title <- T.strip <$> spanP '(' -- TODO: This is wrong!. This parser is dependent on the syntax of tasktime.
             taskTime <- tokenP taskTimeP
             return (Heading title taskTime)

