{-# LANGUAGE TemplateHaskell #-}

module Types ( Put(..)
             ) where

import Relude
import Parser (Parser)

-- TODO: Find better way to implment the syntax!.
class (Put a) where
  put :: a -> Text

class (Parse a) where
  parse :: Parser a
