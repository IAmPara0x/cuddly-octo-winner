{-# LANGUAGE LambdaCase #-}
module Parser ( x
              , Parser(..)
              , stringP
              , intP
              , timeP
              , taskTimeP
              , headingP
              ) where


-- The parser here is inspired by the following paper:
-- Monadic Parsing in haskell (https://www.cmi.ac.in/~spsuresh/teaching/prgh15/papers/monadic-parsing.pdf)

import Data.Bifunctor ( first
                      )

import Data.Char ( isSpace
                 , isDigit
                 )
import Control.Monad
import Control.Applicative

import Types
import Syntax
import Time ( newTime
            , Time
            )

newtype Parser a = Parser { runParser :: String -> Maybe (a, String)
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
item = Parser (\case
                  "" -> Nothing
                  (c:cs) -> Just (c,cs)
               )

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

spanP :: Char -> Parser String
spanP c = Parser (Just . span (/= c))

spaceP :: Parser String
spaceP = many (predP isSpace)

stringP :: String -> Parser String
stringP ""     = return ""
stringP (c:cs) = charP c >> stringP cs >> return (c:cs)

sepbyP :: Parser a -> Parser b -> Parser [a]
sepbyP a sep = many $ a <* sep

tokenP :: Parser a -> Parser a
tokenP aP = do
            a <- aP
            spaceP
            return a

symbP :: String -> Parser String
symbP symbN = tokenP (stringP symbN)

symbCharP :: Char -> Parser Char
symbCharP = tokenP . charP

spanTokenP :: String -> Parser String
spanTokenP = undefined

-- Parsing Tasks
--

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
            title <- spanP taskTimePrefix
            taskTime <- tokenP taskTimeP
            symbP headingSuffix
            return (Heading title taskTime)

descP :: Parser Desc
descP = do
         symbP descPrefix
         return undefined


-- NOTE: Just an example of task
x = Task { taskHeading = Heading "New Heading 200" $ TaskTime (newTime 23 30) Nothing
         , taskDesc = Just $ Desc "just a random description"
         , taskTags = Tags ["Yuno", "Gasai"]
         }
