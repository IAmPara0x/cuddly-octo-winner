module Parser ( x
              , PData(..)
              , Parser(..)
              , spanP
              , charP
              , seqP
              , intP
              , headingP
              , timeP
              , taskTimeP
              , descP
              , wsP
              , tagsP
              , taskP
              ) where


import Data.Bifunctor ( first
                      , second
                      )

import Control.Applicative
import Data.Char ( isDigit
                 , isSpace
                 , digitToInt
                 )

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
    f []          = Nothing
    f (x:xs)
      | x == c    = Just (c, xs)
      | otherwise = Nothing

seqP :: String -> Parser String
seqP = traverse charP

spanC :: Char -> Parser String
spanC c = Parser (Just . span (/= c))

spanP :: String -> Parser String
spanP c = Parser (\input -> cmp ("", input))
  where
    cmp (r,"") = Nothing
    cmp (r,rs)
      | take lenc rs == c = Just (r, drop lenc rs)
      | otherwise         = cmp (r ++ [head rs], tail rs)
    lenc = length c

--- Parse Datas

wsP :: Parser String
wsP = Parser (Just . span isSpace)

nlP :: Parser String
nlP = Parser (Just . span (== '\n'))

intP :: Parser Int
intP = Parser (readInt . span isDigit)
  where
    readInt ("", _) = Nothing
    readInt (x, xs)  = Just (read x::Int, xs)


timeP :: Parser Time
timeP = newTime <$> (intP <* wsP <* spanP "H:") <*>
                    (wsP *> intP <* wsP <* spanP "M")


taskTimeP :: Parser TaskTime
taskTimeP = TaskTime <$> startP <*> (endP <|> Nothing <$ spanP taskTimeSuffix)
  where
    startP = wsP *> spanP taskTimePrefix *> timeP
             <* wsP <* spanP taskTimeSep <* wsP
    endP   = wsP *> (Just <$> timeP) <* wsP <* seqP taskTimeSuffix


headingP :: Parser Heading
headingP = Heading <$> (seqP headingPrefix *> wsP *> spanC '(') <*>
                       (taskTimeP <* spanP headingSuffix <* nlP)

descP :: Parser Desc
descP = Desc <$> (seqP descPrefix *> spanP descSuffix <* nlP)

tagsP :: Parser Tags
tagsP = Tags <$> (seqP tagsPrefix *> many (wsP *> spanP tagsSep) <* wsP <* seqP tagsSuffix <* nlP)

taskP :: Parser Task
taskP = Task <$> headingP <*> (Just <$> descP) <*> tagsP

parser :: Parser PData
parser = undefined


x = Task { taskHeading = Heading "New Heading 200" $ TaskTime (newTime 23 30) Nothing
         , taskDesc = Just $ Desc "just a random description"
         , taskTags = Tags ["Yuno", "Gasai"]
         }
