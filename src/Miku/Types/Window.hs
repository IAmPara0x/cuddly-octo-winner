{-# LANGUAGE StandaloneDeriving #-}
module Miku.Types.Window
  ( Window(..)
  , Windows(..)
  , Layout(..)
  , HorizPos(..)
  , VertPos(..)
  , Lookup
  , type (:#)
  , KnownWindow(..)
  , mapWindows
  )
  where

import Miku.Draw (Draw)
import Relude hiding (Either(..))

data HorizPos = Left | Right
                deriving (Show)
data VertPos  = Top  | Bottom
                deriving (Show)

data Layout = Vert VertPos
            | Horiz HorizPos
            | Stack VertPos HorizPos
            deriving (Show)

type family (v :: VertPos) :# (h :: HorizPos) where
  v :# h = 'Stack v h

data Window (a :: Layout) where
  TopW    :: Window ('Vert 'Top)
  BottomW :: Window ('Vert 'Bottom)
  LeftW   :: Window ('Horiz 'Left)
  RightW  :: Window ('Horiz 'Right)
  (:#)    :: Window ('Vert a) -> Window ('Horiz b) -> Window (a :# b)

deriving stock instance Show (Window a)

data Windows (a :: [(Layout, Type)]) where
  WNil :: Windows '[]
  (:>) :: (Window p, Draw a) -> Windows xs -> Windows ('(p, a) ': xs)

infixr 5 :>

type family Lookup (x :: Layout) (xs :: [(Layout, Type)]) :: Type where
  Lookup x ( '(x, a) ': _ )  = a
  Lookup x ( '(y, a) ': xs ) = Lookup x xs

class KnownWindow x xs where
  window  :: Window x -> Windows xs -> Draw (Lookup x xs)
  modifyW :: Window x -> (Draw (Lookup x xs) -> Draw (Lookup x xs)) -> Windows xs -> Windows xs

instance {-# OVERLAPPING #-} KnownWindow x ('(x, a) ': xs) where
  window _ ((_, a) :> _)    = a
  modifyW _ f ((w,a) :> xs) = (w, f a) :> xs

instance (KnownWindow p xs, Lookup p ('(q, b) ': xs) ~ Lookup p xs) => KnownWindow p ('(q, b) ': xs) where
  window p (_ :> xs)    = window p xs
  modifyW p f (x :> xs) = x :> modifyW p f xs

mapWindows :: (forall a. Draw a -> Draw a) -> Windows xs -> Windows xs
mapWindows _ WNil          = WNil
mapWindows f ((w,a) :> xs) = (w, f a) :> mapWindows f xs
