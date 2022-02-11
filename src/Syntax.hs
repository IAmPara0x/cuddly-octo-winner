{-# LANGUAGE OverloadedStrings #-}

module Syntax ( elemSuffix
              , wSpace
              , newline
              , (+>)
              , (<+)
              , newElem
              , surroundElem
              )
              where

import Relude
import qualified Data.Text as T

wSpace :: Int -> Text
wSpace = flip T.replicate " "

newline :: Int -> Text
newline = flip T.replicate "\n"

elemSuffix :: Text
elemSuffix = "<br>"

(+>) :: Int -> Text -> Text
n +> str =  wSpace n <> str

(<+) :: Text -> Int -> Text
str <+ n = str <> wSpace n

newElem :: Text -> Text
newElem x = T.concat [x, elemSuffix, newline 2]

surroundElem :: Text -> Text -> Text -> Text
surroundElem encS encE x = T.concat [encS, x, encE]

