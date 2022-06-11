{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Miku where

import Relude


run :: IO()
run = print @Text "Moshi Moshi"
