{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercises.Chapter10 where

import Data.Kind (Type)

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
