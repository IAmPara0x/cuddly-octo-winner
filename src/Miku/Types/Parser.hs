module Miku.Types.Parser ( module X
                         , Parser
                         , spaceP
                         ) where


import Data.Void (Void)
import Data.Text (Text)

import Text.Megaparsec         as X hiding (many, some)
import Text.Megaparsec.Char    as X

import Relude

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many spaceChar
