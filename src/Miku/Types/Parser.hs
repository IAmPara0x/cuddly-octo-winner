{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}

module Miku.Types.Parser ( module X
                         , Element(..)
                         , Parser
                         , spaceP
                         , tokenP
                         , (:>>)
                         , PData(..)
                         , RData(..)
                         , Prefix
                         , Digits
                         , Literal
                         , Token
                         , Spaces
                         ) where

import qualified Data.Text as T
import Text.Megaparsec         as X hiding (many, some, Token)
import Text.Megaparsec.Char    as X
import Text.Read (read)

import Relude
import GHC.TypeLits

import Control.Monad.Reader
import Data.Functor.Identity

data Prefix  (p :: Symbol)
data Literal (p :: Symbol)
data Token   (p :: Symbol)
data Digits
data Spaces

data (p :: k1) :>> (a :: k2)
infixr 5 :>>

class RData (a :: Type)  (f :: Type) where
  type RType a f  :: Type
  rdata :: f -> Parser (RType a f)

class PData (a :: Type) where
  type PType a :: Type
  pdata :: Parser (PType a)

instance (KnownSymbol p) => PData (Prefix p) where
  type PType (Prefix p) = ()
  pdata = void $ string (T.pack $ symbolVal @p Proxy) *> spaceP

instance PData Digits where
  type PType Digits = Integer
  pdata = read <$> some digitChar

instance (KnownSymbol p) => PData (Literal p) where
  type PType (Literal p) = ()
  pdata = void $ string (T.pack $ symbolVal @p Proxy)

instance (KnownSymbol p) => PData (Token p) where
  type PType (Token p) = ()
  pdata = void $ spaceP *> string (T.pack $ symbolVal @p Proxy) <* spaceP

instance PData Spaces where
  type PType Spaces = ()
  pdata = void spaceP

----------------------------------------------------------------
-- RData Instances
----------------------------------------------------------------

instance (KnownSymbol p) => RData (Prefix p) (() -> a)  where
  type RType (Prefix p) (() -> a)  = a
  rdata f = f <$> pdata @(Prefix p)
--
instance RData Digits (Integer -> a)  where
  type RType Digits (Integer -> a)  = a
  rdata f = f <$> pdata @Digits

instance (KnownSymbol p) => RData (Literal p) (() -> a) where
  type RType (Literal p) (() -> a) = a
  rdata f = f <$> pdata @(Literal p)

instance (KnownSymbol p) => RData (Token p) (() -> a) where
  type RType (Token p)  (() -> a) = a
  rdata f = f <$> pdata @(Token p)

instance RData Spaces (() -> a) where
  type RType Spaces  (() -> a) = a
  rdata f = f <$> pdata @Spaces

instance (PData p1, PType p1 ~ a, RData p2 b) => RData (p1 :>> p2) (a -> b) where
  type RType (p1 :>> p2) (a -> b) = RType p2 b
  rdata f = pdata @p1 >>= rdata @p2 @b . f

class (RData a f) => Element (a :: Type) (f :: Type)  where
  elementF :: f
  elementP :: Parser (RType a f)
  elementP = rdata @a (elementF @a @f)
  -- parseElement :: Parser a
  -- parseElement

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many $ char ' '

tokenP :: Parser a -> Parser a
tokenP p = spaceP *> p <* spaceP

instance ShowErrorComponent Text where
  showErrorComponent = show
