{-# Language GADTs #-}

module Miku.Data.Types ( Element(..)
                       ) where

import Relude
import qualified Data.Text as T
import Miku.Data.Parser ( Parser
                        , runParser
                        )

class (Element a) where
  prefix :: a -> Maybe Text
  sep    :: a -> Maybe Text
  suffix :: a -> Maybe Text
  parse  :: Parser a
  put    :: a -> Text
  toValue :: Text -> Maybe a
  toValue = fmap fst . runParser parse
  put a = foldMap (fromMaybe "" . ($ a)) [prefix, sep, suffix]

