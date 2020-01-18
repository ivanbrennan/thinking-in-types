{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercises.Chapter10 where

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
