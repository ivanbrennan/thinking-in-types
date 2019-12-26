module Exercises.Chapter3 where

{- Exercise 3-i

   Which of these types are `Functor`s?
   Give instnances for the ones that are.
-}

newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> Int)
newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T1 where
  fmap f (T1 g) = T1 (f . g)

instance Functor T5 where
  fmap f (T5 g) = T5 $ \h -> g (h . f)
