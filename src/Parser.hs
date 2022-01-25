module Parser ( x
              , PData(..)
              , Parser(..)
              , charP
              ) where

import Data.Bifunctor (first,bimap)
import Task
import Time (newTime)

data PData = PHeading Heading
           | PDesc (Maybe Desc)
           | PTags Tags
           | PTaskTime TaskTime
           | PTask Task
           | PList [PData]

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

  {-

        a :: String -> Maybe (a, String)
        ab :: String -> Maybe (a -> b, String)
        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -}

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs)
      | x == c    = Just (c, xs)
      | otherwise = Nothing

parser :: Parser PData
parser = undefined

x = Task { taskHeading = Heading "New Heading 100" $ TaskTime (newTime 23 30) (Just $ newTime 24 00)
         , taskDesc = Just $ Desc "just a random description"
         , taskTags = Tags ["Yuno", "Gasai"]
         }
