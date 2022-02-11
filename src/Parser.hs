{-# LANGUAGE OverloadedStrings #-}

module Parser ( Parser(..)
              , item
              , predP
              , charP
              , intP
              , spanP
              , spaceP
              , stringP
              , sepbyP
              , tokenP
              , symbP
              , symbCharP
              , spanTokenP
              , elemP
              ) where


-- The parser here is inspired by the following paper:
-- Monadic Parsing in haskell (https://www.cmi.ac.in/~spsuresh/teaching/prgh15/papers/monadic-parsing.pdf)

import Relude
import Control.Monad
import Control.Applicative
import qualified Data.Text as T

import Data.Maybe (fromJust)
import Data.Bifunctor (first)

import Data.Char ( isSpace
                 , isDigit
                 )

-- import Miku.Data.Task
-- import Miku.Data.Time
import Syntax

newtype Parser a = Parser { runParser :: Text -> Maybe (a, Text)
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
    parse digits = return (fromJust $ readMaybe digits::Int)

spanP :: Char -> Parser Text
spanP c = Parser (Just . T.span (/= c))

spaceP :: Parser Text
spaceP = T.pack <$> many (predP isSpace)

stringP :: Text -> Parser Text
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

symbP :: Text -> Parser Text
symbP symbN = tokenP (stringP symbN)

symbCharP :: Char -> Parser Char
symbCharP = tokenP . charP

spanTokenP :: Text -> Parser Text
spanTokenP name = do
                    x <- stringP name `mplus` (T.singleton <$> item)
                    if x == name
                       then Parser (\input -> Just ("", T.append x input))
                       else T.append x <$> spanTokenP name

elemP :: Parser a -> Parser a
elemP aP = do
            a <- aP
            symbP elemSuffix
            return a
