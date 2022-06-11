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
{-# LANGUAGE UndecidableInstances  #-}

module Miku.Types.Parser
  ( Atom(..)
  , Composeable(..)
  , type (:>>)
  , type (<:)
  , type (:>)
  , AlphaNum
  , Digits
  , Literal
  , Many
  , Newline
  , Optional
  , Prefix
  , PrintChar
  , Repeat
  , Some
  , Space
  , Tab
  , TakeTill
  , Token
  , Try
  )
  where

import qualified Data.Text as T
import           GHC.TypeLits
import           Text.Megaparsec hiding (Token, many, some)
import           Text.Megaparsec.Char
import           Text.Read (read)
import           Relude hiding (natVal, Alt)


class Composeable (a :: Type) (f :: Type) where
  type ComposeP a f :: Type
  type ComposeS a :: Type

  composeP :: f -> Parser (ComposeP a f)
  composeS :: Text -> ComposeS a

class Atom (a :: Type) where
  type AtomType a :: Type

  parseAtom :: Parser (AtomType a)
  showAtom :: AtomType a -> Text


data (p :: k1) :> (a :: k2)

infixr 5 :>

instance (Atom p1, Monoid (AtomType p1), Atom p2) => Atom (p1 :> p2) where
  type AtomType (p1 :> p2) = AtomType p2

  parseAtom    = parseAtom @p1 >> parseAtom @p2
  showAtom  p2 = showAtom @p1 mempty <> showAtom @p2 p2

instance (Atom p1, Monoid (AtomType p1), Atom p2, AtomType p2 ~ a) => Composeable (p1 :> p2) (a -> b) where
  type ComposeP (p1 :> p2) (a -> b) = b
  type ComposeS (p1 :> p2)          = AtomType p2 -> Text

  composeP f   = parseAtom @p1 >> f <$> parseAtom @p2
  composeS s a = s <> showAtom @(p1 :> p2) a


data (p :: k1) :>> (a :: k2)

infixr 5 :>>

instance (Atom p1, AtomType p1 ~ a, Composeable p2 b) => Composeable (p1 :>> p2) (a -> b) where
  type ComposeP (p1 :>> p2) (a -> b) = ComposeP p2 b
  type ComposeS (p1 :>> p2) = AtomType p1 -> ComposeS p2

  composeP f    = parseAtom @p1 >>= composeP @p2 @b . f
  composeS s p1 = composeS @p2 @b (s <> showAtom @p1 p1)


data (p :: k1) <: (a :: k2)

infixl 6 <:

instance (Atom p1, Atom p2, Monoid (AtomType p2)) => Atom (p1 <: p2) where
  type AtomType (p1 <: p2) = AtomType p1

  parseAtom = do
    x <- parseAtom @p1
    void (parseAtom @p2)
    return x
  showAtom p1 = showAtom @p1 p1 <> showAtom @p2 mempty

instance (Atom p1, AtomType p1 ~ a, Atom p2, Monoid (AtomType p2)) => Composeable (p1 <: p2) (a -> b) where
  type ComposeP (p1 <: p2) (a -> b) = b
  type ComposeS (p1 <: p2)          = AtomType p1 -> Text

  composeP f = do
    b <- f <$> parseAtom @p1
    void (parseAtom @p2)
    return b

  composeS s a = s <> showAtom @(p1 <: p2) a


data AlphaNum

instance Atom AlphaNum where
  type AtomType AlphaNum = Text

  parseAtom = T.pack <$> some alphaNumChar
  showAtom  = id
  
instance Composeable AlphaNum (Text -> a) where
  type ComposeP AlphaNum (Text -> a) = a
  type ComposeS AlphaNum             = Text -> Text

  composeP f   = f <$> parseAtom @AlphaNum
  composeS s a = s <> showAtom @AlphaNum a


data Digits

instance Atom Digits where
  type AtomType Digits = Integer

  parseAtom = read <$> some digitChar
  showAtom  = show
  
instance Composeable Digits (Integer -> a) where
  type ComposeP Digits (Integer -> a) = a
  type ComposeS Digits = Integer -> Text

  composeP f   = f <$> parseAtom @Digits
  composeS s i = s <> showAtom @Digits i


data Literal (p :: Symbol)

instance (KnownSymbol p) => Atom (Literal p) where
  type AtomType (Literal p) = ()

  parseAtom   = void  $ string (T.pack $ symbolVal @p Proxy)
  showAtom () = T.pack (symbolVal @p Proxy)

instance (KnownSymbol p) => Composeable (Literal p) (() -> a) where
  type ComposeP (Literal p) (() -> a) = a
  type ComposeS (Literal p)           = Text

  composeP f = f <$> parseAtom @(Literal p)
  composeS s = s <> showAtom @(Literal p) ()


data Many (p :: Type)

instance (Atom p) => Atom (Many p) where
  type AtomType (Many p) = [AtomType p]

  parseAtom = many (parseAtom @p)
  showAtom  []    = ""
  showAtom (x:xs) = showAtom @p x <> showAtom @(Many p) xs

instance (Atom p, [AtomType p] ~ a) => Composeable (Many p) (a -> b) where
  type ComposeP (Many p) (a -> b) = b
  type ComposeS (Many p)          = [AtomType p] -> Text

  composeP f   = f <$> parseAtom @(Many p)
  composeS s a = s <> showAtom @(Many p) a


data Newline

instance Atom Newline where
  type AtomType Newline = ()

  parseAtom   = void eol
  showAtom () = "\n"
  
instance Composeable Newline (() -> a) where
  type ComposeP Newline (() -> a) = a
  type ComposeS Newline           = Text

  composeP f = f <$> parseAtom @Newline
  composeS s = s <> "\n"


data Optional (p :: Type)

instance (Atom p) => Atom (Optional p) where
  type AtomType (Optional p) = Maybe (AtomType p)

  parseAtom         = optional (parseAtom @p)
  showAtom Nothing  = ""
  showAtom (Just a) = showAtom @p a
  
instance (Atom p, Maybe (AtomType p) ~ a) => Composeable (Optional p) (a -> b) where
  type ComposeP (Optional p) (a -> b) = b
  type ComposeS (Optional p)          = Maybe (AtomType p) -> Text

  composeP f          = f <$> parseAtom @(Optional p)
  composeS s Nothing  = s
  composeS s (Just a) = s <> showAtom @p a

data Prefix (p :: Symbol)

instance (KnownSymbol p) => Atom (Prefix p) where
  type AtomType (Prefix p) = ()

  parseAtom   = void  $ string (T.pack $ symbolVal @p Proxy) *> spaceP
  showAtom () = T.pack (symbolVal @p Proxy) <> " "

instance (KnownSymbol p) => Composeable (Prefix p) (() -> a) where
  type ComposeP (Prefix p) (() -> a) = a
  type ComposeS (Prefix p)           = Text

  composeP f = f <$> parseAtom @(Prefix p)
  composeS s = s <> showAtom @(Prefix p) ()


data PrintChar

instance Atom PrintChar where
  type AtomType PrintChar = Text

  parseAtom = T.pack <$> some printChar
  showAtom  = id
  
instance Composeable PrintChar (Text -> a) where
  type ComposeP PrintChar (Text -> a) = a
  type ComposeS PrintChar             = Text -> Text

  composeP f   = f <$> parseAtom @PrintChar
  composeS s a = s <> showAtom @PrintChar a


data Repeat (n :: Nat) (p :: Type)

instance (Atom p, Monoid (AtomType p), KnownNat n) => Atom (Repeat n p) where
  type AtomType (Repeat n p) = ()

  parseAtom = case natVal @n Proxy of
                0 -> return ()
                n -> replicateM_ (fromInteger n) (void $ parseAtom @p)
  showAtom () = case natVal @n Proxy of
                  0 -> ""
                  n -> mconcat $ replicate (fromInteger n) $ showAtom @p mempty

instance (Atom p, Monoid(AtomType p), KnownNat n) => Composeable (Repeat n p) (() -> b) where
  type ComposeP (Repeat n p) (() -> b) = b
  type ComposeS (Repeat n p)           = Text

  composeP f  = f <$> parseAtom @(Repeat n p)
  composeS s  = s <> showAtom @(Repeat n p) ()


data Some (p :: Type)

instance (Atom p) => Atom (Some p) where
  type AtomType (Some p) = [AtomType p]

  parseAtom = some (parseAtom @p)
  showAtom  []    = ""
  showAtom (x:xs) = showAtom @p x <> showAtom @(Some p) xs

instance (Atom p, [AtomType p] ~ a) => Composeable (Some p) (a -> b) where
  type ComposeP (Some p) (a -> b) = b
  type ComposeS (Some p)          = [AtomType p] -> Text

  composeP f   = f <$> parseAtom @(Some p)
  composeS s a = s <> showAtom @(Some p) a


data Space

instance Atom Space where
  type AtomType Space = ()

  parseAtom   = void (char ' ')
  showAtom () = " "
  
instance Composeable Space (() -> a) where
  type ComposeP Space (() -> a) = a
  type ComposeS Space           = Text

  composeP f = f <$> parseAtom @Space
  composeS s = s <> showAtom @Space ()


data Tab

instance Atom Tab where
  type AtomType Tab = ()

  -- TODO: check how to interpret tab/space.
  parseAtom   = void tab
  showAtom () = "  "

instance Composeable Tab (() -> a) where
  type ComposeP Tab (() -> a) = a
  type ComposeS Tab           = Text

  composeP f = f <$> parseAtom @Tab
  composeS s = s <> showAtom @Tab ()


data TakeTill (p :: Symbol)

instance (KnownSymbol p) => Atom (TakeTill p) where
  type AtomType (TakeTill p) = Text

  parseAtom =
    case symbolVal @p Proxy of
      [c] -> takeWhileP (Just "Take till c") (/= c)
      s   -> customFailure $ "a just a single character instead got: " <> T.pack s
  showAtom = id
  
instance (KnownSymbol p) => Composeable (TakeTill p) (Text -> a) where
  type ComposeP (TakeTill p) (Text -> a) = a
  type ComposeS (TakeTill p)             = Text -> Text

  composeP f   = f <$> parseAtom @(TakeTill p)
  composeS s a = s <> showAtom @(TakeTill p) a


data Token (p :: Symbol)

instance (KnownSymbol p) => Atom (Token p) where
  type AtomType (Token p) = ()

  parseAtom    = void  $ spaceP *> string (T.pack $ symbolVal @p Proxy)
  showAtom  () = T.pack (symbolVal @p Proxy)

instance (KnownSymbol p) => Composeable (Token p) (() -> a) where
  type ComposeP (Token p) (() -> a) = a
  type ComposeS (Token p)           = Text

  composeP f = f <$> parseAtom @(Token p)
  composeS s = s <> showAtom @(Token p) ()


data Try (p :: Type)

instance (Atom p) => Atom (Try p) where
  type AtomType (Try p) = (AtomType p)

  parseAtom = try (parseAtom @p)
  showAtom  = showAtom @p

----------------------------------------------------------------
-- Misc
----------------------------------------------------------------

type Parser = Parsec Text Text

spaceP :: Parser String
spaceP = many $ char ' '

instance ShowErrorComponent Text where
  showErrorComponent = show

