{-# LANGUAGE ConstraintKinds    #-}
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

import Miku.Draw (Draw)
import Relude    hiding (Either (..))

data HorizPos
  = Left
  | Right
  deriving stock (Show)
data VertPos
  = Top
  | Bottom
  deriving stock (Show)

data Layout
  = Vert VertPos
  | Horiz HorizPos
  | Stack VertPos HorizPos
  deriving stock (Show)

type family (v :: VertPos) :# (h :: HorizPos) where
  v :# h = 'Stack v h

data Window (a :: Layout) where
  WTop :: Window ('Vert 'Top)
  WBottom :: Window ('Vert 'Bottom)
  WLeft :: Window ('Horiz 'Left)
  WRight :: Window ('Horiz 'Right)
  (:#) :: Window ('Vert a) -> Window ('Horiz b) -> Window (a :# b)

deriving stock instance Show (Window a)

data Windows (c :: Type -> Constraint) (a :: [(Layout, Type)]) where
  WNil :: () => Windows c '[]
  (:>) :: (c a) => (Window p, Draw a) -> Windows c xs -> Windows c ('(p, a) : xs)

infixr 5 :>

type family Lookup (x :: Layout) (xs :: [(Layout, Type)]) :: Type where
  Lookup x ( '(x, a) ': _ )  = a
  Lookup x ( '(y, a) ': xs ) = Lookup x xs

class KnownWindow x xs where
  wGet  :: Window x -> Windows c xs -> Draw (Lookup x xs)
  wModify :: Window x -> (Draw (Lookup x xs) -> Draw (Lookup x xs)) -> Windows c xs -> Windows c xs

instance {-# OVERLAPPING #-} KnownWindow x ('(x, a) ': xs) where
  wGet _ ((_, a) :> _)    = a
  wModify _ f ((w,a) :> xs) = (w, f a) :> xs

instance (KnownWindow p xs, Lookup p ('(q, b) ': xs) ~ Lookup p xs)
  => KnownWindow p ('(q, b) ': xs) where
  wGet p (_ :> xs)    = wGet p xs
  wModify p f (x :> xs) = x :> wModify p f xs

wMap :: (forall a. Draw a -> Draw a) -> Windows c xs -> Windows c xs
wMap _ WNil          = WNil
wMap f ((w,a) :> xs) = (w, f a) :> wMap f xs

wToList :: forall b xs c. (forall a. c a => Draw a -> b) -> Windows c xs -> [b]
wToList _ WNil           = []
wToList f ((_, a) :> xs) = f a : wToList f xs
