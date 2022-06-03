{-# LANGUAGE TypeApplications #-}

module Miku (run) where

import Relude

run :: IO()
run = print @Text "fresh start"
