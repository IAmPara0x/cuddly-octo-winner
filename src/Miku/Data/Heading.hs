{-# LANGUAGE TemplateHaskell #-}

module Miku.Data.Heading ( Heading(Heading)
                         , titleL
                         , taskTimeL
                         , headingP
                         )
                         where
import Relude hiding (put)
import Control.Lens (makeLenses)
import qualified Data.Text as T

import Parser
import Types
import Syntax

import Miku.Data.TaskTime

data Heading = Heading { _titleL    :: Text
                       , _taskTimeL :: TaskTime
                       }
               deriving (Show)

makeLenses ''Heading
--
-- Syntax for heading
headingPrefix :: Text
headingPrefix = "#### Task:"


instance Element Heading where
  prefix                  = const $ Just (headingPrefix <+2)
  sep (Heading name time) = Just $ name <+ 2 <> put time
  suffix                  = const $ Just (elemSuffix <> newline 2)
  parse                   = headingP

headingP :: Parser Heading
headingP = do
             symbP headingPrefix
             title <- T.strip <$> spanP '(' -- TODO: This is wrong!. This parser is dependent on the syntax of tasktime.
             taskTime <- tokenP parse
             return (Heading title taskTime)

