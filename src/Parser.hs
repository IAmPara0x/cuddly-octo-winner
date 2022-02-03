{-# LANGUAGE OverloadedStrings #-}

module Parser ( Parser(..)
              , tasksP
              ) where


-- The parser here is inspired by the following paper:
-- Monadic Parsing in haskell (https://www.cmi.ac.in/~spsuresh/teaching/prgh15/papers/monadic-parsing.pdf)

import Control.Monad
import Control.Applicative
import qualified Data.Text as T

import Data.Bifunctor (first)

import Data.Char ( isSpace
                 , isDigit
                 )

import Types
import Syntax
import Time ( newTime
            , Time
            )

newtype Parser a = Parser { runParser :: T.Text -> Maybe (a, T.Text)
                          }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure = return
  (<*>) = ap

-- Monad Laws:
-- 1. (return x) >>= f == f x
-- 2. m >>= return == m
-- 3. (m >>= f) >>= g == m >>= (\x -> f x >>= g)

instance Monad Parser where
  return a = Parser (\input -> Just (a, input))
  (Parser p) >>= f  = Parser (\input -> do
                               (a, input') <- p input
                               runParser (f a) input'
                              )

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

-- MonadPlus Laws:
-- 1. mzero >>= f == mzero
-- 2. m >>= (\x -> mzero) == mzero
-- 3. mzero `mplus` m == m
-- 4. m `mplus` mzero == m

instance MonadPlus Parser where
  mzero = Parser (const Nothing)
  mplus (Parser aP) (Parser bP) = Parser (\input -> aP input <|> bP input)

--- Implementation ---

-- Utility Functions

item :: Parser Char
item = Parser parse
  where
    parse "" = Nothing
    parse input = Just (T.head input, T.tail input)


predP :: (Char -> Bool) -> Parser Char
predP p = item >>= (\c -> if p c
                            then return c
                            else mzero
                    )

charP :: Char -> Parser Char
charP c = predP (c ==)

intP :: Parser Int
intP = many (predP isDigit) >>= parse
  where
    parse "" = mzero
    parse digits = return (read digits::Int)

spanP :: Char -> Parser T.Text
spanP c = Parser (Just . T.span (/= c))

spaceP :: Parser T.Text
spaceP = T.pack <$> many (predP isSpace)

stringP :: T.Text -> Parser T.Text
stringP ""     = return ""
stringP input = charP c >> stringP cs >> return input
  where
    c = T.head input
    cs = T.tail input

sepbyP :: Parser a -> Parser b -> Parser [a]
sepbyP a sep = many $ a <* sep

tokenP :: Parser a -> Parser a
tokenP aP = do
            a <- aP
            spaceP
            return a

symbP :: T.Text -> Parser T.Text
symbP symbN = tokenP (stringP symbN)

symbCharP :: Char -> Parser Char
symbCharP = tokenP . charP

spanTokenP :: T.Text -> Parser T.Text
spanTokenP name = do
                    x <- stringP name `mplus` (T.singleton <$> item)
                    if x == name
                       then Parser (\input -> Just ("", T.append x input))
                       else T.append x <$> spanTokenP name


-- Parsing Tasks

timeP :: Parser Time
timeP = newTime <$> tokenP intP <* symbP "H:" <*>
                    tokenP intP <* symbCharP 'M'

taskTimeP :: Parser TaskTime
taskTimeP = do
              symbCharP taskTimePrefix
              startT <- tokenP timeP
              symbCharP taskTimeSep
              endT <- (Just <$> tokenP timeP <* symbCharP taskTimeSuffix) `mplus`
                      (Nothing <$ symbCharP taskTimeSuffix)
              return (TaskTime startT endT)


headingP :: Parser Heading
headingP = do
             symbP headingPrefix
             title <- T.strip <$> spanP taskTimePrefix
             taskTime <- tokenP taskTimeP
             symbP headingSuffix
             return (Heading title taskTime)

descP :: Parser Desc
descP = do
          symbP descPrefix
          descStr <- spanTokenP descSuffix
          symbP descSuffix
          return (Desc $ T.strip descStr)

tagsP :: Parser Tags
tagsP = do
          symbP tagsPrefix
          tags <- T.splitOn (T.singleton tagsSep) <$> spanTokenP tagsSuffix
          symbP tagsSuffix
          return (Tags $ map T.strip tags)

taskP :: Parser Task
taskP = do
          heading <- headingP
          desc <- (Just <$> descP) `mplus`
                  (Nothing <$ return "")
          tags <- tagsP
          symbP taskSep
          return $ Task heading desc tags

tasksP :: Parser [Task]
tasksP = many taskP

