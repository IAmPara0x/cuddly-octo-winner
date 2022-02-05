{-# LANGUAGE OverloadedStrings #-}

module Syntax ( timeHrs
              , timeMins
              , elemSuffix
              , headingPrefix
              , taskTimePrefix
              , taskTimeSuffix
              , taskTimeSep
              , tagsPrefix
              , tagsSuffix
              , tagsSep
              , descPrefix
              , taskSep
              , wSpace
              , newline
              , (+>)
              , (<+)
              , newElem
              , surroundElem
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

elemSuffix :: T.Text
elemSuffix = "<br>"

headingPrefix :: T.Text
headingPrefix = "#### Task:"

taskTimePrefix :: Char
taskTimePrefix  = '('

taskTimeSuffix :: Char
taskTimeSuffix  = ')'

taskTimeSep :: Char
taskTimeSep = '-'

tagsPrefix :: T.Text
tagsPrefix = "`Tags:"

tagsSuffix :: T.Text
tagsSuffix = "`"

tagsSep :: Char
tagsSep = ','

descPrefix :: T.Text
descPrefix = "Desc:"

taskSep :: T.Text
taskSep = T.replicate 8 "-"

(+>) :: Int -> T.Text -> T.Text
n +> str =  T.append (wSpace n) str

(<+) :: T.Text -> Int -> T.Text
str <+ n = T.append str (wSpace n)

newElem :: T.Text -> T.Text
newElem x = T.concat [x, elemSuffix, newline 2]

surroundElem :: T.Text -> T.Text -> T.Text -> T.Text
surroundElem encS encE x = T.concat [encS, x, encE]

