{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Exercises.Chapter4 where

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep)

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

{-
  typeName @Bool
  "Bool"

  typeName @String
  "[Char]"

  typeName @(Maybe [Int])
  "Maybe [Int]"
-}
