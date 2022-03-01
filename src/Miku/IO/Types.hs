{-# Language GADTs #-}
{-# Language FlexibleContexts #-}

module Miku.IO.Types ( Miku(..)
                     , Msg(..)
                     , EitherIO(..)
                     , CmdL(..)
                     , msg
                     , createM
                     , execM
                     ) where

import Relude hiding (show)
import Text.Show (Show(..))
import Control.Monad.Trans.Except (ExceptT)

type EitherIO a  = forall b. ExceptT (Msg b) IO a
type Miku a      = forall f. Functor f => (a -> Msg a -> f a) -> EitherIO (f a)

data (Msg t) where
  Err :: Msg t
  Suc :: Msg t
  Msg :: Msg t -> String -> Msg t

msg :: Msg t -> String -> Msg t
msg Err = Msg Err
msg Suc = Msg Suc


instance CmdL t => Show (Msg t) where
  show (Msg t m) = show t <> m
  show t         = prefixMsg t

class (CmdL a) where
  readM   :: Miku a
  newM    :: Miku a
  writeM  :: a -> Miku a

  prefixMsg  :: Msg a -> String
  
  readL :: EitherIO a
  readL = runIdentity <$> readM (const . Identity)

  newL :: EitherIO a
  newL = runIdentity <$> newM (const . Identity)

  writeL :: a -> EitherIO a
  writeL a = runIdentity <$> writeM a (const . Identity)

execM :: CmdL a => Miku a -> EitherIO (Msg a)
execM cmd = getConst <$> cmd (const Const)

createM :: EitherIO a -> Msg a -> Miku a
createM either_a m f = (`f` m) <$> either_a

