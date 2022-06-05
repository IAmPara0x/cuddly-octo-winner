{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Miku.Types.Parser where

import qualified Data.Text as T
import           GHC.TypeLits
import           Text.Megaparsec hiding (Token, many, some)
import           Text.Megaparsec.Char
import           Text.Read (read)
import           Relude


data Prefix (p :: Symbol)

data Literal (p :: Symbol)

data Token (p :: Symbol)

data Some (p :: Type)

data Many (p :: Type)

data Digits

data AlphaNums

data Space

data Newline

data Optional (p :: Type)

data TakeTill (p :: Symbol)

data (p :: k1) :> (a :: k2)

infixr 5 :>

data (p :: k1) :>> (a :: k2)

infixr 5 :>>

data (p :: k1) <: (a :: k2)

infixl 6 <:

data (p :: k1) <<: (a :: k2)

infixl 5 <<:

class Composeable (a :: Type) (f :: Type) where
  type CType a f :: Type
  composeP :: f -> Parser (CType a f)

class Atom (a :: Type) where
  type AtomType a :: Type
  atomP :: Parser (AtomType a)

----------------------------------------------------------------
-- Atom Instances
----------------------------------------------------------------

instance (KnownSymbol p) => Atom (Prefix p) where
  type AtomType (Prefix p) = ()
  atomP = void $ string (T.pack $ symbolVal @p Proxy) *> spaceP

instance Atom Digits where
  type AtomType Digits = Integer
  atomP = read <$> some digitChar

instance (KnownSymbol p) => Atom (Literal p) where
  type AtomType (Literal p) = ()
  atomP = void $ string (T.pack $ symbolVal @p Proxy)

instance (KnownSymbol p) => Atom (Token p) where
  type AtomType (Token p) = ()
  atomP = void $ spaceP *> string (T.pack $ symbolVal @p Proxy) <* spaceP

instance Atom Space where
  type AtomType Space = ()
  atomP = void (char ' ')

instance (Atom p) => Atom (Some p) where
  type AtomType (Some p) = [AtomType p]
  atomP = some (atomP @p)

instance (Atom p) => Atom (Many p) where
  type AtomType (Many p) = [AtomType p]
  atomP = many (atomP @p)

instance Atom Newline where
  type AtomType Newline = ()
  atomP = void eol

instance Atom AlphaNums where
  type AtomType AlphaNums = Text
  atomP = T.pack <$> some alphaNumChar

instance (Atom p) => Atom (Optional p) where
  type AtomType (Optional p) = Maybe (AtomType p)
  atomP = optional (atomP @p)

instance (KnownSymbol p) => Atom (TakeTill p) where
  type AtomType (TakeTill p) = Text
  atomP =
    case symbolVal @p Proxy of
      [c] -> takeWhileP (Just "Take till c") (/= c)
      s   -> customFailure $ "a just a single character instead got: " <> T.pack s

instance (Atom p1, Atom p2) => Atom (p1 <: p2) where
  type AtomType (p1 <: p2) = AtomType p1
  atomP = do
    x <- atomP @p1
    void (atomP @p2)
    return x

instance (Atom p1, Atom p2) => Atom (p1 :> p2) where
  type AtomType (p1 :> p2) = AtomType p2
  atomP = atomP @p1 >> atomP @p2

----------------------------------------------------------------
-- Composeable Instances
----------------------------------------------------------------

instance (KnownSymbol p) => Composeable (Prefix p) a where
  type CType (Prefix p) a = a
  composeP a = atomP @(Prefix p) >> return a

instance Composeable Digits (Integer -> a) where
  type CType Digits (Integer -> a) = a
  composeP f = f <$> atomP @Digits

instance (KnownSymbol p) => Composeable (Literal p) (() -> a) where
  type CType (Literal p) (() -> a) = a
  composeP f = f <$> atomP @(Literal p)

instance (KnownSymbol p) => Composeable (Token p) (() -> a) where
  type CType (Token p) (() -> a) = a
  composeP f = f <$> atomP @(Token p)

instance Composeable Space (() -> a) where
  type CType Space (() -> a) = a
  composeP f = f <$> atomP @Space

instance (Atom p, [AtomType p] ~ a) => Composeable (Some p) (a -> b) where
  type CType (Some p) (a -> b) = b
  composeP f = f <$> atomP @(Some p)

instance (Atom p, [AtomType p] ~ a) => Composeable (Many p) (a -> b) where
  type CType (Many p) (a -> b) = b
  composeP f = f <$> atomP @(Many p)

instance Composeable Newline (() -> a) where
  type CType Newline (() -> a) = a
  composeP f = f <$> atomP @Newline

instance Composeable AlphaNums (Text -> a) where
  type CType AlphaNums (Text -> a) = a
  composeP f = f <$> atomP @AlphaNums

instance (Atom p, Maybe (AtomType p) ~ a) => Composeable (Optional p) (a -> b) where
  type CType (Optional p) (a -> b) = b
  composeP f = f <$> atomP @(Optional p)

instance (KnownSymbol p) => Composeable (TakeTill p) (Text -> a) where
  type CType (TakeTill p) (Text -> a) = a
  composeP f = f <$> atomP @(TakeTill p)

instance (Atom p1, AtomType p1 ~ a, Composeable p2 b) => Composeable (p1 :>> p2) (a -> b) where
  type CType (p1 :>> p2) (a -> b) = CType p2 b
  composeP f = atomP @p1 >>= composeP @p2 @b . f

instance (Composeable p1 a, Atom p2, Atom p1) => Composeable (p1 <: p2) a where
  type CType (p1 <: p2) a = CType p1 a
  composeP a = do
    x <- composeP @p1 @a a
    void (atomP @p2)
    return x

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many $ char ' '

tokenP :: Parser a -> Parser a
tokenP p = spaceP *> p <* spaceP

instance ShowErrorComponent Text where
  showErrorComponent = show
