{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Miku.Types.Task
  ( Task (Task),
    TaskP,
    taskName,
    taskStart,
    taskEnd,
  )
where

import qualified Data.Text as T
import Miku.Types.Parser
import Miku.Types.Time
import Relude hiding (show)
import Text.Show (Show (..))

data Task = Task
  { taskName :: Text,
    taskStart :: Time,
    taskEnd :: Maybe Time
  } deriving (Show)

type TaskP =  (Prefix "###" :> TakeTill "(" <: Token "(")
          :>> TimeP <: Token "-"
          :>> Optional TimeP <: Token ")" <: Some Newline
              <: Literal "---" <: Some Newline

instance Atom TaskP where
  type AtomType TaskP = Task
  atomP = composeP @TaskP Task
