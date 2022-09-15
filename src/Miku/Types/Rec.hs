{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
module Miku.Types.Rec
  ( Elem
  , Field (..)
  , Rec (..)
  , Unit
  , recToList
  , rfilterMap
  , rfmap
  , rmap
  ) where

import Relude

type Rec :: [Type -> Constraint] -> [Type] -> (Type -> Type) -> Type
data Rec cs xs f where
  RNil :: Rec cs '[] f
  (:>) :: AllC cs x => f x -> Rec cs xs f -> Rec cs (x : xs) f
infixr 5 :>

type family AllC (cs :: [Type -> Constraint]) (x :: Type) :: Constraint where
  AllC '[] x       = ()
  AllC (c ': cs) x = (c x, AllC cs x)

type Field :: Type -> [Type] -> Constraint
class (Elem x xs ~ 'True) => Field x xs where
  rget :: Rec cs xs f -> f x
  rput :: f x -> Rec cs xs f -> Rec cs xs f
  rmodify :: (f x -> f x) -> Rec cs xs f -> Rec cs xs f

instance {-# OVERLAPPING #-} (Elem x (x ': xs) ~ 'True) => Field x (x ': xs) where
  rget (x :> _)       = x
  rput x' (_ :> xs)   = x' :> xs
  rmodify f (x :> xs) = f x :> xs

instance (Elem x (y ': xs) ~ 'True, Field x xs) => Field x (y ': xs) where
  rget (_ :> xs)       = rget xs
  rput x' (y :> xs)    = y :> rput x' xs
  rmodify f (y :> xs)  = y :> rmodify f xs

type family (Elem x xs) :: Bool where
  Elem x '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

type family Fun (xs :: [Type]) :: [Type] where
  Fun '[]       = '[]
  Fun (x ': xs) = (x -> x) ': Fun xs


class Unit a
instance Unit a

rmap :: (forall x. f x -> f x) -> Rec cs xs f -> Rec cs xs f
rmap _ RNil      = RNil
rmap f (x :> xs) = f x :> rmap f xs

rfmap :: Functor f => (forall x. f x -> Bool)
      -> Rec '[Unit] (Fun xs) Identity
      -> Rec cs xs f
      -> Rec cs xs f
rfmap _ _ RNil              = RNil
rfmap p (f :> fs) (x :> xs)
  | p x                     = fmap (runIdentity f) x :> rfmap p fs xs
  | otherwise               = x :> rfmap p fs xs

recToList :: (forall x. AllC cs x => f x -> a) -> Rec cs xs f -> [a]
recToList _ RNil      = []
recToList f (x :> xs) = f x : recToList f xs

rfilterMap :: (forall x. AllC cs x => f x -> Bool)
           -> (forall x. AllC cs x => f x -> a)
           -> Rec cs xs f
           -> [a]
rfilterMap _ _ RNil      = []
rfilterMap p f (x :> xs)
  | p x       = f x : rfilterMap p f xs
  | otherwise = rfilterMap p f xs
