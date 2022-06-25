{-# LANGUAGE ScopedTypeVariables #-}

module Exercises.Chapter1 where

import Data.Proxy (Proxy(Proxy))

{- Exercise 1.2-i

   Determine the cardinality of:
    Either Bool (Bool, Maybe Bool) -> Bool

   |Bool|
     = 2

   |Maybe Bool|
     = 1 + |Bool|
     = 1 + 2
     = 3

   |(Bool, Maybe Bool)|
     = |Bool| x |Maybe Bool|
     = 2 x 3
     = 6

   |Either Bool (Bool, Maybe Bool)|
     = |Bool| + |(Bool, Maybe Bool)|
     = 2 + 6
     = 8

   |a -> b| = |b| x |b| x ... x |b| = |b| ^ |a|
                    |a| times

   |Either Bool (Bool, Maybe Bool) -> Bool|
     = |Bool| ^ |Either Bool (Bool, Maybe Bool)|
     = 2 ^ 8
     = 256
-}

class Cardinality a where
  cardinality :: Proxy a -> Int

instance Cardinality Bool where
  cardinality _ = 2

instance Cardinality a => Cardinality (Maybe a) where
  cardinality _ = 1 + cardinality (Proxy :: Proxy a)

instance (Cardinality a, Cardinality b) => Cardinality (a, b) where
  cardinality _ =
    cardinality (Proxy :: Proxy a) *
    cardinality (Proxy :: Proxy b)

instance (Cardinality a, Cardinality b) => Cardinality (Either a b) where
  cardinality _ =
    cardinality (Proxy :: Proxy a) +
    cardinality (Proxy :: Proxy b)

instance (Cardinality a, Cardinality b) => Cardinality (a -> b) where
  cardinality _ =
    cardinality (Proxy :: Proxy b) ^
    cardinality (Proxy :: Proxy a)

answer_1_2_i :: Int
answer_1_2_i =
  cardinality (Proxy :: Proxy (Either Bool (Bool, Maybe Bool) -> Bool))

{- Exercise 1.4-i

   Use Curry–Howard to prove the exponent law that:
     (a ^ b) × (a ^ c) = a ^ (b + c)

   That is, provide a function of the type:
     (b -> a) -> (c -> a) -> Either b c -> a
   and one of:
     (Either b c -> a) -> (b -> a, c -> a)

  Algebra:
    (a ^ b) × (a ^ c) = a ^ (b + c)

  Types:
    (b -> a, c -> a) isomorphic (Either b c -> a)
-}

answer_1_4_i_to :: (b -> a) -> (c -> a) -> Either b c -> a
answer_1_4_i_to f1 f2 = f
  where
    -- This is 'Prelude.either'.
    f (Left x) = f1 x
    f (Right x) = f2 x

answer_1_4_i_from :: (Either b c -> a) -> (b -> a, c -> a)
answer_1_4_i_from f = (f1, f2)
  where
    f1 = f . Left
    f2 = f . Right

{- Exercise 1.4-ii

  Algebra:
    (a × b) ^ c = (a ^ c) × (b ^ c)

  Types:
    (c -> (a, b)) isomorphic (c -> a, c -> b)
-}

answer_1_4_ii_to :: (c -> (a, b)) -> (c -> a, c -> b)
answer_1_4_ii_to f = (f1, f2)
  where
    f1 = fst . f
    f2 = snd . f

answer_1_4_ii_from :: (c -> a, c -> b) -> (c -> (a, b))
answer_1_4_ii_from (f1, f2) = f
  where
    f x = (f1 x, f2 x)

{- Exercise 1.4-ii

  Algebra:
    (a ^ b) ^ c = a ^ (b x c)

  Types:
    (c -> b -> a) isomorphic (b -> c -> a)
-}

answer_1_4_iii_to :: (c -> b -> a) -> (b -> c -> a)
answer_1_4_iii_to f = f'
  where
    -- This is 'Prelude.flip'.
    f' x y = f y x

answer_1_4_iii_from :: (b -> c -> a) -> (c -> b -> a)
answer_1_4_iii_from f = f'
  where
    -- This is 'Prelude.flip'.
    f' x y = f y x
