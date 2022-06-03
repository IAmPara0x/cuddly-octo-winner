{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Miku.Types.Parser ( module X
                         , Element(..)
                         , Parser
                         , spaceP
                         , tokenP
                         ) where

import Text.Megaparsec         as X hiding (many, some)
import Text.Megaparsec.Char    as X

import Relude

class Element (a :: *) where
  parseElement :: Parser a
  putElement   :: a -> Text

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many $ char ' '

tokenP :: Parser a -> Parser a
tokenP p = spaceP *> p <* spaceP

instance ShowErrorComponent Text where
  showErrorComponent = show
