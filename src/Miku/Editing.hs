{-# LANGUAGE StandaloneDeriving #-}
module Miku.Editing
  ( EditingMode (Normal, Insert)
  , SEditingMode (SNormal, SInsert)
  ) where

import Relude

data EditingMode
  = Normal
  | Insert
  deriving stock (Show)

data SEditingMode (a :: EditingMode) where
  SNormal :: SEditingMode 'Normal
  SInsert :: SEditingMode 'Insert

deriving stock instance Show (SEditingMode a)
