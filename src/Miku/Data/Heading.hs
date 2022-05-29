{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Miku.Data.Heading (Parser, Heading, pHeading) where

import Relude
import Prelude (Read(..))
import Data.Void (Void)

import Text.Show (Show)

import qualified Text.Megaparsec as P
import Text.Megaparsec.Char

import Data.Time


type Parser = P.Parsec Void Text

newtype Heading = Heading { getHeading :: Day }
                  deriving newtype (Show, Read)

headingPrefix :: (IsString a) => a
headingPrefix  = "# Date: "

pHeading :: Parser Heading
pHeading = do
  void (string headingPrefix)
  void (many $ char ' ')

  year  <- some numberChar <* char '-'
  month <- some numberChar <* char '-'
  day   <- some numberChar

  let mday  = readMaybe @Heading (intercalate "-" [year, month, day])
  
  return $ fromMaybe (error "Error: Not able to parse the day.") mday
