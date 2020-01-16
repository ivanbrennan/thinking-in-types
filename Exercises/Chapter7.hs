{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercises.Chapter7 where

import Data.Foldable (asum)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)

{- Exercise 7.1-i

   Are functions of type forall a. a -> r interesting? Why or why not?

   Answer: Such functions can only return constant values, since they
   don't know anything about their input and thus cannot vary their output
   on the basis of input.
-}

answer_7_1_i :: forall a. a -> Char
answer_7_1_i = const 'x'

{- Exercise 7.1-ii -}

data HasShow where
  HasShow :: Show t => t -> HasShow

{-
   What happens to this instance if you remove the Show t => constraint from HasShow?

     instance Show HasShow where
       show (HasShow s) = "HasShow " ++ show s

   Answer: No instance for (Show t) arising from a use of `show'
-}

{- Exercise 7.1-iii

   Write the Show instance for HasShow in terms of elimHasShow.
-}

elimHasShow
  :: (forall a. Show a => a -> r)
  -> HasShow
  -> r
elimHasShow f (HasShow a) = f a

instance Show HasShow where
  show x = "HasShow " ++ elimHasShow show x

{- Dynamic Types -}

data Dynamic where
  Dynamic :: Typeable a => a -> Dynamic

elimDynamic
  :: (forall a. Typeable a => a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
  :: forall a b r.
     ( Typeable a
     , Typeable b
     , Typeable r
     )
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f =
  fmap Dynamic . f
    <$> fromDynamic @a d1
    <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int    @Int    a b (+)
    , liftD2 @String @Int    a b $ \strA intB ->
        strA ++ show intB
    , liftD2 @Int @String    a b $ \intA strB ->
        show intA ++ strB
    ]

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r
elimHas f (Has a) = f a

type HasShow' = Has Show
type Dynamic' = Has Typeable

isMempty
  :: (Monoid a, Eq a)
  => a
  -> Bool
isMempty a = a == mempty

class    (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a
