{-# LANGUAGE FlexibleContexts #-}
module Miku.Types.Parser ( module X
                         , Parser
                         , spaceP
                         , tokenP
                         ) where


import Data.Void (Void)
import Data.Text (Text)

import Text.Megaparsec         as X hiding (many, some)
import Text.Megaparsec.Char    as X

import Relude

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many $ char ' '

tokenP :: Parser a -> Parser a
tokenP p = spaceP *> p <* spaceP

instance ShowErrorComponent Text where
  showErrorComponent = show
