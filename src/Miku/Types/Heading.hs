{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Miku.Types.Heading ( Parser
                          , Heading(Heading, getHeading)
                          , HeadingP
                          , HeadingF
                          , DayP
                          , DayF
                          , dayP
                          ) where

import qualified Data.Text as T
import           Text.Read (read)
import           Text.Show (Show(..))
import           Data.Time (Day)

import           Relude    hiding (show)

import           Miku.Types.Parser


newtype Heading = Heading { getHeading :: Day }
                  deriving newtype (Read)

type DayP = Digits :>> Literal "-" :>> Digits :>> Literal "-" :>> Digits
type DayF = Integer -> () -> Integer -> () -> Integer -> Day

dayP :: Integer -> () -> Integer -> () -> Integer -> Day
dayP y () m () d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

instance Element DayP DayF where 
  elementF = dayP

type HeadingP = Prefix "# Date: " :>> DayP :>> Spaces
type HeadingF = () -> Integer -> () -> Integer -> () -> Integer -> () -> Day


instance Show Heading where
  show (Heading date) = headingPrefix <> show date <> "\n\n"
--
-- instance Element Heading where
--   parseElement = headingP
--   putElement   = T.pack . show

headingPrefix :: (IsString a) => a
headingPrefix  = "# Date: "

-- headingP :: Parser Heading
-- headingP = do
--   void (string headingPrefix)
--   void spaceP
--
--   year  <- some digitChar <* char '-' <?> "Year"
--   month <- some digitChar <* char '-' <?> "month"
--   day   <- some digitChar <?> "day"
--
--   void (many $ char ' ')
--   void eol
--
--   let mdate  = readMaybe @Heading (intercalate "-" [year, month, day])
--
--   case mdate of
--     (Just date) -> return date
--     Nothing     -> customFailure "Expected valid a heading like: 2022-05-27" 
