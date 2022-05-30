{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Miku.Types.Heading ( Parser
                          , Heading(Heading)
                          , headingP
                          ) where

import           Data.Char (isNumber)
import           Text.Show (Show)
import           Data.Time (Day)
import           Data.Text (unpack)

import           Relude

import           Miku.Types.Parser


newtype Heading = Heading { getHeading :: Day }
                  deriving newtype (Show, Read)

headingPrefix :: (IsString a) => a
headingPrefix  = "# Date: "


headingP :: Parser Heading
headingP = do
  void (string headingPrefix)
  void spaceP

  year  <- some numberChar <* char '-' <?> "Year"
  month <- some numberChar <* char '-' <?> "month"
  day   <- unpack <$> takeWhileP (Just "day in the format of: 05.") isNumber

  void eol

  let mdate  = readMaybe @Heading (intercalate "-" [year, month, day])

  case mdate of
    (Just date) -> return date
    Nothing     -> customFailure "Expected valid a heading like: 2022-05-27" 
