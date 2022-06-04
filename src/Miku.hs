{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Miku where

import Miku.Types.Parser
import Relude



run :: IO()
run = print @Text "fresh start"
