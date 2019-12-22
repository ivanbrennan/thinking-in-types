{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Exercises.Chapter2 where

import Control.Monad.Trans.Class (MonadTrans)
import Data.Kind (Type, Constraint)

{- Exercise 2.1.3-i

   If `Show Int` has kind CONSTRAINT, what’s the kind of `Show`?
-}

type Answer_2_1_3_i =
  (Show :: Type -> Constraint)

{- Exercise 2.1.3-ii

   What is the kind of `Functor`?
-}

type Answer_2_1_3_ii =
  (Functor :: (Type -> Type) -> Constraint)

{- Exercise 2.1.3-iii

   What is the kind of `Monad`?
-}

type Answer_2_1_3_iii =
  (Monad :: (Type -> Type) -> Constraint)

{- Exercise 2.1.3-iv

   What is the kind of `MonadTrans`?
-}

type Answer_2_1_3_iv =
  (MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint)

{- Exercise 2.4-i

   Write a closed type family to compute Not.
-}

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True
