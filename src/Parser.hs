module Parser ( x
              , PData(..)
              , Parser(..)
              , spanP
              , charP
              , seqP
              , headingP
              , timeP
              , taskTimeP
              ) where


import Data.Bifunctor ( first
                      , second
                      )

import Control.Applicative

import Types
import Time ( newTime
            , Time
            )

data PData = PList [PData]
           deriving (Show)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String)
                          }

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (\x -> Just (a, x))
  (Parser abP) <*> (Parser aP)   = Parser (\input -> do
                                           (ab,input') <- abP input
                                           (a,input'') <- aP input'
                                           Just (ab a , input'')
                                           )

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) (Parser p1) (Parser p2) = Parser (\input -> p1 input <|> p2 input)


charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs)
      | x == c    = Just (c, xs)
      | otherwise = Nothing

seqP :: String -> Parser String
seqP = traverse charP

spanP :: String -> Parser String
spanP c = Parser $ Just . second (drop lenc) . (\input -> cmp ("", input))
  where
    cmp (r,"") = (r,"")
    cmp (r,rs)
      | take lenc rs == c = (r,rs)
      | otherwise         = cmp (r ++ [head rs], tail rs)
    lenc = length c


--- Parse Datas
--

timeP :: Parser Time
timeP = (\a b -> newTime (read a::Int) (read b::Int)) <$> spanP "H:" <*> spanP "M"


taskTimeP :: Parser TaskTime
taskTimeP = undefined

headingP :: Parser String
headingP = seqP headingPrefix *> spanP headingSuffix


parser :: Parser PData
parser = undefined

x = Task { taskHeading = Heading "New Heading 200" $ TaskTime (newTime 23 30) (Just $ newTime 24 00)
         , taskDesc = Just $ Desc "just a random description"
         , taskTags = Tags ["Yuno", "Gasai"]
         }
