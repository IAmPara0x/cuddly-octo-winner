module Syntax ( headingPrefix
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

wSpace :: Int -> String
wSpace = flip replicate ' '

newline :: Int -> String
newline = flip replicate '\n'

headingPrefix :: String
headingPrefix = "#### Task:"

headingSuffix :: String
headingSuffix = "<br>"

taskTimePrefix :: Char
taskTimePrefix  = '('

taskTimeSuffix :: Char
taskTimeSuffix  = ')'

taskTimeSep :: Char
taskTimeSep = '-'

tagsPrefix :: String
tagsPrefix = "`Tags:"

tagsSuffix :: String
tagsSuffix = "`<br>"

tagsSep :: Char
tagsSep = ','

descPrefix :: String
descPrefix = "Desc:"

descSuffix :: String
descSuffix = "<br>"

taskSep :: String
taskSep = replicate 8 '-'

(+>) :: Int -> String -> String
n +> str = wSpace n ++ str


(<+) :: String -> Int -> String
(<+) str n = str ++ wSpace n
