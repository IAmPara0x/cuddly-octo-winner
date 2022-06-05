{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Miku.Types.Heading
  ( Parser,
    Heading (Heading, getHeading),
    HeadingP,
    DayP,
    dayP,
    headingP,
  )
where

import qualified Data.Text as T
import Data.Time (Day)
import Miku.Types.Parser
import Relude hiding (show)
import Text.Read (read)
import Text.Show (Show (..))


type DayP = Digits <: Literal "-" :>> Digits <: Literal "-" :>> Digits

dayP :: Integer -> Integer -> Integer -> Day
dayP y m d = read (show y <> "-" <> m' <> "-" <> d')
  where
    m' = if m < 10 then "0" <> show m else show m
    d' = if d < 10 then "0" <> show d else show d

instance Atom DayP where
  type AtomType DayP = Day
  atomP = composeP @DayP dayP

newtype Heading = Heading {getHeading :: Day}
  deriving (Show)

type HeadingP = (Prefix "# Date: " :> DayP <: Many Space) :>> Some Newline

headingP :: Day -> [()] -> Heading
headingP day _ = Heading day

instance Atom HeadingP where
  type AtomType HeadingP = Heading
  atomP = composeP @HeadingP headingP
