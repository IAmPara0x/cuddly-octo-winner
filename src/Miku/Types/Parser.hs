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

module Miku.Types.Parser
  ( Prefix
  , Literal
  , Token
  , Some
  , Many
  , Digits
  , PrintChar
  , Space
  , Newline
  , Optional
  , TakeTill
  , Tab
  , type (:>>)
  , type (<:)
  , type (:>)
  , Atom(..)
  , Composeable(..)
  )
  where

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

data PrintChar

data Space

data Newline

data Tab

data Optional (p :: Type)

data TakeTill (p :: Symbol)

data (p :: k1) :> (a :: k2)

infixr 5 :>

data (p :: k1) :>> (a :: k2)

infixr 5 :>>

data (p :: k1) <: (a :: k2)

infixl 6 <:

class Composeable (a :: Type) (f :: Type) where
  type ComposeP a f :: Type
  composeP :: f -> Parser (ComposeP a f)

class Atom (a :: Type) where
  type AtomP a :: Type
  atomP :: Parser (AtomP a)

----------------------------------------------------------------
-- Atom Instances
----------------------------------------------------------------

instance (KnownSymbol p) => Atom (Prefix p) where
  type AtomP (Prefix p) = ()
  atomP = void  $ string (T.pack $ symbolVal @p Proxy) *> spaceP

instance Atom Digits where
  type AtomP Digits = Integer
  atomP = read <$> some digitChar

instance (KnownSymbol p) => Atom (Literal p) where
  type AtomP (Literal p) = ()
  atomP = void  $ string (T.pack $ symbolVal @p Proxy)

instance (KnownSymbol p) => Atom (Token p) where
  type AtomP (Token p) = ()
  atomP = void  $ spaceP *> string (T.pack $ symbolVal @p Proxy) <* spaceP

instance Atom Space where
  type AtomP Space = ()
  atomP = void (char ' ')

instance (Atom p) => Atom (Some p) where
  type AtomP (Some p) = [AtomP p]
  atomP = some (atomP @p)

instance (Atom p) => Atom (Many p) where
  type AtomP (Many p) = [AtomP p]
  atomP = many (atomP @p)

instance Atom Newline where
  type AtomP Newline = ()
  atomP = void eol

instance Atom PrintChar where
  type AtomP PrintChar = Text
  atomP = T.pack <$> some printChar

instance (Atom p) => Atom (Optional p) where
  type AtomP (Optional p) = Maybe (AtomP p)
  atomP = optional (atomP @p)

instance (KnownSymbol p) => Atom (TakeTill p) where
  type AtomP (TakeTill p) = Text
  atomP =
    case symbolVal @p Proxy of
      [c] -> takeWhileP (Just "Take till c") (/= c)
      s   -> customFailure $ "a just a single character instead got: " <> T.pack s

instance (Atom p1, Atom p2) => Atom (p1 <: p2) where
  type AtomP (p1 <: p2) = AtomP p1
  atomP = do
    x <- atomP @p1
    void (atomP @p2)
    return x

instance (Atom p1, Atom p2) => Atom (p1 :> p2) where
  type AtomP (p1 :> p2) = AtomP p2
  atomP = atomP @p1 >> atomP @p2

instance Atom Tab where
  type AtomP Tab = ()
  atomP = void tab

----------------------------------------------------------------
-- Composeable Instances
----------------------------------------------------------------

instance (KnownSymbol p) => Composeable (Prefix p) (() -> a) where
  type ComposeP (Prefix p) (() -> a) = a
  composeP f = f <$> atomP @(Prefix p)
  -- composeS = 

instance Composeable Digits (Integer -> a) where
  type ComposeP Digits (Integer -> a) = a
  composeP f = f <$> atomP @Digits

instance (KnownSymbol p) => Composeable (Literal p) (() -> a) where
  type ComposeP (Literal p) (() -> a) = a
  composeP f = f <$> atomP @(Literal p)

instance (KnownSymbol p) => Composeable (Token p) (() -> a) where
  type ComposeP (Token p) (() -> a) = a
  composeP f = f <$> atomP @(Token p)

instance Composeable Space (() -> a) where
  type ComposeP Space (() -> a) = a
  composeP f = f <$> atomP @Space

instance (Atom p, [AtomP p] ~ a) => Composeable (Some p) (a -> b) where
  type ComposeP (Some p) (a -> b) = b
  composeP f = f <$> atomP @(Some p)

instance (Atom p, [AtomP p] ~ a) => Composeable (Many p) (a -> b) where
  type ComposeP (Many p) (a -> b) = b
  composeP f = f <$> atomP @(Many p)

instance Composeable Newline (() -> a) where
  type ComposeP Newline (() -> a) = a
  composeP f = f <$> atomP @Newline

instance Composeable PrintChar (Text -> a) where
  type ComposeP PrintChar (Text -> a) = a
  composeP f = f <$> atomP @PrintChar

instance (Atom p, Maybe (AtomP p) ~ a) => Composeable (Optional p) (a -> b) where
  type ComposeP (Optional p) (a -> b) = b
  composeP f = f <$> atomP @(Optional p)

instance (KnownSymbol p) => Composeable (TakeTill p) (Text -> a) where
  type ComposeP (TakeTill p) (Text -> a) = a
  composeP f = f <$> atomP @(TakeTill p)

instance (Atom p1, AtomP p1 ~ a, Composeable p2 b) => Composeable (p1 :>> p2) (a -> b) where
  type ComposeP (p1 :>> p2) (a -> b) = ComposeP p2 b
  composeP f = atomP @p1 >>= composeP @p2 @b . f

instance (Atom p1, AtomP p1 ~ a, Atom p2) => Composeable (p1 <: p2) (a -> b) where
  type ComposeP (p1 <: p2) (a -> b) = b
  composeP f = do
    b <- f <$> atomP @p1
    void (atomP @p2)
    return b

instance (Atom p1, Atom p2, AtomP p2 ~ a) => Composeable (p1 :> p2) (a -> b) where
  type ComposeP (p1 :> p2) (a -> b) = b
  composeP f = atomP @p1 >> f <$> atomP @p2

instance Composeable Tab (() -> a) where
  type ComposeP Tab (() -> a) = a
  composeP f = f <$> atomP @Tab

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many $ char ' '

instance ShowErrorComponent Text where
  showErrorComponent = show
