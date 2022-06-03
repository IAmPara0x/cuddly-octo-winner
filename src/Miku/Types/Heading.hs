{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Miku.Types.Heading ( Parser
                          , Heading(Heading, getHeading)
                          ) where

import qualified Data.Text as T
import           Text.Show (Show(..))
import           Data.Time (Day)

import           Relude    hiding (show)

import           Miku.Types.Parser


newtype Heading = Heading { getHeading :: Day }
                  deriving newtype (Read)

instance Show Heading where
  show (Heading date) = headingPrefix <> show date <> "\n\n"

instance Element Heading where
  parseElement = headingP
  putElement   = T.pack . show

headingPrefix :: (IsString a) => a
headingPrefix  = "# Date: "

headingP :: Parser Heading
headingP = do
  void (string headingPrefix)
  void spaceP

  year  <- some digitChar <* char '-' <?> "Year"
  month <- some digitChar <* char '-' <?> "month"
  day   <- some digitChar <?> "day"

  void (many $ char ' ')
  void eol

  let mdate  = readMaybe @Heading (intercalate "-" [year, month, day])

  case mdate of
    (Just date) -> return date
    Nothing     -> customFailure "Expected valid a heading like: 2022-05-27" 
