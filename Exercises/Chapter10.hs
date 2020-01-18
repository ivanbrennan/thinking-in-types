{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercises.Chapter10 where

import Data.Kind (Constraint, Type)

newtype Fst a b = Fst (a, b)

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

{- Exercise 10.1-i

   Defunctionalize listToMaybe :: [a] -> Maybe a
-}

newtype ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe [])      = Nothing
  eval (ListToMaybe (a : _)) = Just a

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb b => Eval (MapList dfb a) [b] where
  eval (MapList _ []) = []
  eval (MapList f (a : as)) =
    eval (f a) : eval (MapList f as)

type Exp a = a -> Type

type family Eval_ (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b
type instance Eval_ (Snd '(_, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval_ (FromMaybe a 'Nothing)  = a
type instance Eval_ (FromMaybe _ ('Just a)) = a

{- Exercise 10.2-i

   Defunctionalize listToMaybe at the type-level.
-}

data ListToMaybe_ :: [a] -> Exp (Maybe a)
type instance Eval_ (ListToMaybe_ '[])      = 'Nothing
type instance Eval_ (ListToMaybe_ (a ': _)) = 'Just a

data MapList_ :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval_ (MapList_ _ '[]) = '[]
type instance Eval_ (MapList_ f (a ': as)) =
  Eval_ (f a) : Eval_ (MapList_ f as)

{- Exercise 10.2-ii

   Defunctionalize foldr ::
   (a -> b -> b) -> b -> [a] -> b
-}

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval_ (Foldr _ z '[]) = z
type instance Eval_ (Foldr f z (a ': as)) =
  Eval_ (f a (Eval_ (Foldr f z as)))

data Pure :: a -> Exp a
type instance Eval_ (Pure a) = a

data (=<<)
  :: (a -> Exp b)
  -> Exp a
  -> Exp b
type instance Eval_ (k =<< e) =
  Eval_ (k (Eval_ e))
infixr 0 =<<

data (<=<)
  :: (b -> Exp c)
  -> (a -> Exp b)
  -> a -> Exp c
type instance Eval_ ((f <=< g) x) =
  Eval_ (f (Eval_ (g x)))
infixr 1 <=<

data TyEq :: a -> b -> Exp Bool
type instance Eval_ (TyEq a b) = TyEqImpl a b
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl _ _ = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval_ (Collapse '[]) = (() :: Constraint)
type instance Eval_ (Collapse (a ': as)) =
  (a, Eval_ (Collapse as))

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval_ (Pure1 f x) = f x

type All_ (c :: k -> Constraint) (ts :: [k]) =
  Collapse =<< MapList_ (Pure1 c) ts

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval_ (Map f '[]) = '[]
type instance Eval_ (Map f (a ': as)) =
  Eval_ (f a) ': Eval_ (Map f as)

type instance Eval_ (Map f 'Nothing)  = 'Nothing
type instance Eval_ (Map f ('Just a)) = 'Just (Eval_ (f a))

type instance Eval_ (Map f ('Left e))  = 'Left e
type instance Eval_ (Map f ('Right a)) = 'Right (Eval_ (f a))

{- Exercise 10.4-i

   Write a promoted functor instance for tuples.
-}

type instance Eval_ (Map f '(a, b)) = '(a, Eval_ (f b))

type family ((a :: [k]) :++ (b :: [k])) :: [k] where
  '[] :++ xs = xs
  (a ': as) :++ xs = a ': (as :++ xs)

data Mappend :: a -> a -> Exp a
type instance Eval_
  (Mappend '() '()) = '()
type instance Eval_
  (Mappend (a :: Constraint)
           (b :: Constraint)) = (a, b)
type instance Eval_
  (Mappend (a :: [k])
           (b :: [k])) = a :++ b

data Mempty :: k -> Exp k
type instance Eval_ (Mempty '()) = '()
type instance Eval_ (Mempty (c :: Constraint)) = (() :: Constraint)
type instance Eval_ (Mempty (_ :: [k])) = '[]
