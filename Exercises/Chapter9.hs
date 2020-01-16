{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Exercises.Chapter9 where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

data (a :: k1) :>> (b :: k2)
infixl 5 :>>

class HasPrintf a where
  type Printf a :: Type
  format :: String
         -> Proxy a
         -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)

type Ex0 = Printf (Int :<< ":" :<< Bool :<< "!")
type Ex1 = Int -> Printf (":" :<< Bool :<< "!")
type Ex2 = Int -> Printf (Bool :<< "!")
type Ex3 = Int -> Bool -> Printf "!"
type Ex4 = Int -> Bool -> String

eq1, eq2, eq3, eq4 :: Bool
eq1 = case (undefined :: Ex0 :~: Ex1) of Refl -> True
eq2 = case (undefined :: Ex1 :~: Ex2) of Refl -> True
eq3 = case (undefined :: Ex2 :~: Ex3) of Refl -> True
eq4 = case (undefined :: Ex3 :~: Ex4) of Refl -> True

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

helloWorld :: String
helloWorld = printf (Proxy @(String :<< " world!")) "hello"

fakeMath :: String
fakeMath = printf (Proxy @(Int :<< "+" :<< Int :<< "=3")) 1 2
