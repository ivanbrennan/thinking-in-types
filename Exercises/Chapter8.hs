{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RoleAnnotations #-}

module Exercises.Chapter8 where

{- Exercise 8.2-i

   What is the role signature of Either a b?

   Answer: representational
-}

data Either' a b = Left' a | Right' b

type role Either' representational representational

{- Exercise 8.2-ii

   What is the role signature of Proxy a?

   Answer: phantom
-}

data Proxy' = forall a. Proxy' a

type role Proxy'
