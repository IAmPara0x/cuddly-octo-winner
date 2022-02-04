{-# LANGUAGE OverloadedStrings #-}

module Syntax ( timeHrs
              , timeMins
              , headingPrefix
              , headingSuffix
              , taskTimePrefix
              , taskTimeSuffix
              , taskTimeSep
              , tagsPrefix
              , tagsSuffix
              , tagsSep
              , descPrefix
              , descSuffix
              , taskSep
              , wSpace
              , newline
              , (+>)
              , (<+)
              )
              where

import qualified Data.Text as T

wSpace :: Int -> T.Text
wSpace = flip T.replicate " "

newline :: Int -> T.Text
newline = flip T.replicate "\n"

timeHrs :: T.Text
timeHrs = "H:"

timeMins :: T.Text
timeMins = "M"


headingPrefix :: T.Text
headingPrefix = "#### Task:"

headingSuffix :: T.Text
headingSuffix = "<br>"

taskTimePrefix :: Char
taskTimePrefix  = '('

taskTimeSuffix :: Char
taskTimeSuffix  = ')'

taskTimeSep :: Char
taskTimeSep = '-'

tagsPrefix :: T.Text
tagsPrefix = "`Tags:"

tagsSuffix :: T.Text
tagsSuffix = "`<br>"

tagsSep :: Char
tagsSep = ','

descPrefix :: T.Text
descPrefix = "Desc:"

descSuffix :: T.Text
descSuffix = "<br>"

taskSep :: T.Text
taskSep = T.replicate 8 "-"

(+>) :: Int -> T.Text -> T.Text
n +> str =  T.append (wSpace n) str


(<+) :: T.Text -> Int -> T.Text
str <+ n = T.append str (wSpace n)
