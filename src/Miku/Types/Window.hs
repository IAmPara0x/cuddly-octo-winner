{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Miku.Types.Window
  ( SWindow(..)
  , Windows(..)
  , KnownWindow(..)
  , Layout(..)
  , HorizPos(..)
  , VertPos(..)
  , mapWindows
  )
  where

import Miku.Draw (Draw, defDraw)
import Relude hiding (Either(..))

data HorizPos = Left | Right
data VertPos  = Top  | Bottom

data Layout = Vert VertPos
            | Horiz HorizPos
            | Stack VertPos HorizPos

data SWindow (a :: Layout) where
  TopW    :: SWindow ('Vert 'Top)
  BottomW :: SWindow ('Vert 'Bottom)
  LeftW   :: SWindow ('Horiz 'Left)
  RightW  :: SWindow ('Horiz 'Right)
  (:#)    :: SWindow ('Vert a) -> SWindow ('Horiz b) -> SWindow ('Stack a b)

data Windows (a :: [(Layout, Type)]) where
  WNil :: Windows '[]
  (:>) :: (SWindow p, Draw a) -> Windows xs -> Windows ('(p, a) ': xs)

infixr 5 :>

type family Lookup (x :: Layout) (xs :: [ (Layout, Type)]) :: Type where
  Lookup x ( '(x, a) ': _ )  = a
  Lookup x ( '(y, a) ': xs ) = Lookup x xs


class KnownWindow p xs where
  window  :: SWindow p -> Windows xs -> Draw (Lookup p xs)
  modifyW :: SWindow p -> (Draw (Lookup p xs) -> Draw (Lookup p xs)) -> Windows xs -> Windows xs

instance {-# OVERLAPPING #-} KnownWindow p ('(p, a) ': xs) where
  window _ ((_, a) :> _)    = a
  modifyW _ f ((w,a) :> xs) = (w, f a) :> xs

instance (KnownWindow p xs, Lookup p ('(q, b) ': xs) ~ Lookup p xs) => KnownWindow p ('(q, b) ': xs) where
  window p (_ :> xs)    = window p xs
  modifyW p f (x :> xs) = x :> modifyW p f xs

mapWindows :: (forall a. Draw a -> Draw a) -> Windows xs -> Windows xs
mapWindows _ WNil          = WNil
mapWindows f ((w,a) :> xs) = (w, f a) :> mapWindows f xs
