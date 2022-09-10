{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
module Miku.Types.Window
  ( HorizPos (..)
  , KnownWindow (..)
  , Layout (..)
  , Lookup
  , VertPos (..)
  , Window (..)
  , Windows (..)
  , type (:#)
  , wMap
  , wToList
  ) where

import Miku.Draw (Draw, Drawable)
import Relude    hiding (Either (..))

data HorizPos
  = Left
  | Right
  deriving (Show)
data VertPos
  = Top
  | Bottom
  deriving (Show)

data Layout
  = Vert VertPos
  | Horiz HorizPos
  | Stack VertPos HorizPos
  deriving (Show)

type family (v :: VertPos) :# (h :: HorizPos) where
  v :# h = 'Stack v h

data Window (a :: Layout) where
  WTop :: Window ('Vert 'Top)
  WBottom :: Window ('Vert 'Bottom)
  WLeft :: Window ('Horiz 'Left)
  WRight :: Window ('Horiz 'Right)
  (:#) :: Window ('Vert a) -> Window ('Horiz b) -> Window (a :# b)

deriving stock instance Show (Window a)

data Windows (a :: [(Layout, Type)]) where
  WNil :: Windows '[]
  (:>) :: Drawable a => (Window p, Draw a) -> Windows xs -> Windows ('(p, a) : xs)

infixr 5 :>

type family Lookup (x :: Layout) (xs :: [(Layout, Type)]) :: Type where
  Lookup x ( '(x, a) ': _ )  = a
  Lookup x ( '(y, a) ': xs ) = Lookup x xs

class KnownWindow x xs where
  wGet  :: Window x -> Windows xs -> Draw (Lookup x xs)
  wModify :: Window x -> (Draw (Lookup x xs) -> Draw (Lookup x xs)) -> Windows xs -> Windows xs

instance {-# OVERLAPPING #-} KnownWindow x ('(x, a) ': xs) where
  wGet _ ((_, a) :> _)    = a
  wModify _ f ((w,a) :> xs) = (w, f a) :> xs

instance (KnownWindow p xs, Lookup p ('(q, b) ': xs) ~ Lookup p xs) => KnownWindow p ('(q, b) ': xs) where
  wGet p (_ :> xs)    = wGet p xs
  wModify p f (x :> xs) = x :> wModify p f xs

wMap :: (forall a. Draw a -> Draw a) -> Windows xs -> Windows xs
wMap _ WNil          = WNil
wMap f ((w,a) :> xs) = (w, f a) :> wMap f xs

wToList :: forall b xs. (forall a. Drawable a => Draw a -> b) -> Windows xs -> [b]
wToList _ WNil           = []
wToList f ((_, a) :> xs) = f a : wToList f xs
