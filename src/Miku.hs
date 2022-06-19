{-# LANGUAGE TypeApplications #-}
module Miku where

import Relude


run :: IO()
run = print @String "moshi moshi"
