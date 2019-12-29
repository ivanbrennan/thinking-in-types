{-# LANGUAGE RankNTypes #-}

module Exercises.Chapter6 where

import Control.Monad.Trans.Class (MonadTrans, lift)

{- Exercise 6.3-i

   What is the rank of:
     Int -> forall a. a -> a

   Hint: try adding the explicit parentheses.

   Int -> forall a. a -> a
   forall a. Int -> a -> a

   Answer: Rank 1
-}

answer_6_3_i :: Int -> (forall a. (a -> a))
answer_6_3_i = const id

{- Exercise 6.3-ii

   What is the rank of:
     (a -> b) -> (forall c. c -> a) -> b

   Hint: recall that the function arrow is right-associative, so
     a -> b -> c
   is actually parsed as
     a -> (b -> c)

   (a -> b) -> (forall c. c -> a) -> b
   forall a b. (a -> b) -> (forall c. c -> a) -> b

   Answer: Rank 2
-}

answer_6_3_ii :: (a -> b) -> (forall c. c -> a) -> b
answer_6_3_ii f g = (f . g) undefined

{- Exercise 6.3-iii

   What is the rank of:
     ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
     forall m b z a. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a

   Believe it or not, this is a real type signature we had to
   write back in the bad old days before MonadUnliftIO!

   Answer: Rank 3
-}

answer_6_3_iii :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
answer_6_3_iii _ = undefined


newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

{- Exercise 6.4-i

   Provide a Functor instance for Cont.

   Hint: use lots of type holes, and an explicit lambda whenever
   looking for a function type.

   The implementation is sufficiently difficult that trying to write
   it point-free will be particularly mind-bending.
-}

instance Functor Cont where
  fmap ab (Cont withA) =
    Cont $ \fromB ->
      withA $ \a ->
        fromB (ab a)

{- Exercise 6.4-ii

   Provide the Applicative instances for Cont.
-}

instance Applicative Cont where
  pure a =
    Cont $ \fromA -> fromA a

  Cont withAB <*> Cont withA =
    Cont $ \fromB ->
      withAB $ \ab ->
        withA $ \a ->
          fromB (ab a)

{- Exercise 6.4-iii

   Provide the Monad instances for Cont.
-}

instance Monad Cont where
  Cont withA >>= toContB =
    Cont $ \fromB ->
      withA $ \a ->
        unCont (toContB a) fromB

{- Exercise 6.4-iv

   There is also a monad transformer version of Cont. Implement it.
-}

newtype ContT m a = ContT
  { unContT :: forall r. (a -> m r) -> m r
  }

instance MonadTrans ContT where
  lift ma =
    ContT $ \fromA ->
      ma >>= fromA

instance Functor (ContT m) where
  fmap ab (ContT withA) =
    ContT $ \fromB ->
      withA $ \a ->
        fromB (ab a)

instance Applicative (ContT m) where
  pure a =
    ContT $ \fromA ->
      fromA a

  ContT withAB <*> ContT withA =
    ContT $ \fromB ->
      withAB $ \ab ->
        withA $ \a ->
          fromB (ab a)

instance Monad (ContT m) where
  ContT withA >>= toContTB =
    ContT $ \fromB ->
      withA $ \a ->
        unContT (toContTB a) fromB
