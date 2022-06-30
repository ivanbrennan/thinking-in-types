{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercises.Chapter5 where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

{-
instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs
-}

{- Exercise 5.3-i

   Implement Ord for HList.

instance Ord (HList '[]) where
  compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare (a :# as) (b :# bs) =
    compare a b <> compare as bs
-}

{- Exercise 5.3-ii

   Implement Show for HList.

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (x :# xs) = show x <> " :# " <> show xs
-}

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All _ '[]       = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

{- Exercise 5.3-iii

   Rewrite the Ord and Show instances in terms of All.
-}

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) =
    compare a b <> compare as bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (x :# xs) = show x <> " :# " <> show xs
