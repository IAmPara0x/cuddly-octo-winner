module Types ( Element(..)
             ) where

import Relude
import qualified Data.Text as T
import Parser (Parser)

class (Element a) where
  prefix :: a -> Maybe Text
  sep    :: a -> Maybe Text
  suffix :: a -> Maybe Text
  parse  :: Parser a
  put    :: a -> Text
  put a = foldMap (fromMaybe "" . ($ a)) [prefix, sep, suffix]
