{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Exercises.Chapter11 where

import Data.Kind ( Type )
import Data.Proxy ( Proxy(Proxy) )
import qualified Data.Vector as V
import Fcf ( Eval, Exp, Filter, FindIndex, FromMaybe, Fst, Lookup, Map
           , Null , Pure, SetIndex, Stuck, TyEq, type (=<<), type (<=<) )
import GHC.TypeLits ( KnownNat, Nat, Symbol, natVal )
import Unsafe.Coerce ( unsafeCoerce )

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum
    :: Int
    -> f t
    -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck
    =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing

decompose
  :: OpenSum f (t ': ts)
  -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

{- Exercise 11.2-i

   Write weaken :: OpenSum f ts -> OpenSum f (x ': ts)
-}

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

match
  :: forall f ts b
   . (forall t. f t -> b)
  -> OpenSum f ts
  -> b
match fn (UnsafeOpenSum _ t) = fn t

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type)
                 (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts

insert
  :: Eval (UniqueKey key ts) ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

type FindElem' (key :: Symbol) (ts :: [(Symbol, t)])
  = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem'
  :: forall key ts
   . KnownNat (FindElem' key ts)
  => Int
findElem' =
  fromIntegral . natVal $ Proxy @(FindElem' key ts)

type LookupType (key :: k) (ts :: [(k, t)])
  = FromMaybe Stuck =<< Lookup key ts

get
  :: forall key ts f
   . KnownNat (FindElem' key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) =
    unAny $ V.unsafeIndex v $ findElem' @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)])
  = SetIndex (FindElem' key ts) '(key, t) ts

update
  :: forall key ts f t
   . KnownNat (FindElem' key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem' @key @ts, Any ft)]

{- Exercise 11.3-i

   Implement delete for OpenProducts.
-}

type family Delete (key :: Symbol) (ts :: [(Symbol, k)]) :: [(Symbol, k)] where
  Delete _   '[]               = '[]
  Delete key ('(key, _) ': ts) = ts
  Delete key (x         ': ts) = x ': Delete key ts

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)])
  = Pure (Delete key ts)

delete
  :: forall key ts f
   . KnownNat (FindElem' key ts)
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
    OpenProduct $ V.take i v V.++ V.drop (i + 1) v
  where
    i = findElem' @key @ts

{- Exercise 11.3-ii

   Implement upsert (update or insert) for OpenProducts.

   Hint: write a type family to compute a MAYBE NAT corresponding
   to the index of the key in the list of types, if it exists.
   Use class instances to lower this kind to the term-level, and
   then pattern match on it to implement upsert.
-}

type ElemIndex (key :: Symbol) (ts :: [(Symbol, k)])
  = Eval (FindIndex (TyEq key <=< Fst) ts)

type UpsertElem (key :: Symbol)
                (t :: k)
                (ts :: [(Symbol, k)]) =
  FromMaybe ('(key, t) ': ts)
    =<< Map (Placeholder1Of3 SetIndex '(key, t) ts)
    =<< FindIndex (TyEq key <=< Fst) ts

data Placeholder1Of3
  :: (a -> b -> c -> Exp r)
  -> b
  -> c
  -> a
  -> Exp r

type instance Eval (Placeholder1Of3 f b c a) = Eval (f a b c)

type UpsertLoc (key :: Symbol)
               (ts :: [(Symbol, k)]) =
  Eval (FindIndex (TyEq key <=< Fst) ts)

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem =
    Just . fromIntegral . natVal $ Proxy @n

upsert
  :: forall key ts t f
   . FindUpsertElem (UpsertLoc key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpsertElem key t ts))
upsert _ ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(UpsertLoc key ts) of
    Nothing -> V.cons (Any ft) v
    Just i -> v V.// [(i, Any ft)]

type family Foo (x :: Type) (y :: Type) :: Type where
  Foo Int Bool = Double
  Foo Int Double = Maybe Char
  Foo String (Maybe Int) = Maybe String
  Foo String (Maybe a) = Either Int a
