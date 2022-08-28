module Miku.Editing (EMode(Normal, Insert)) where

import Relude

data EMode = Normal | Insert
             deriving stock (Show)
