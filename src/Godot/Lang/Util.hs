{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Godot.Lang.Util where

import Data.Kind (Type)

import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), Meta (..), U1, K1, (:*:) (..))

-- | Max Width (max number of fields of any product)
class                    MW (f :: Type -> Type) where mW :: Proxy f -> Int
instance (MW f, MW g) => MW (f :+: g)           where mW _ = max (mW (Proxy @f)) (mW (Proxy @g))
instance (MW f, MW g) => MW (f :*: g)           where mW _ = mW (Proxy @f) + mW (Proxy @g)
instance                 MW (K1 i c)            where mW _ = 1
instance MW f         => MW (M1 i t f)          where mW _ = mW (Proxy @f)
instance                 MW U1                  where mW _ = 0

-- | Dummy type used for GHC.Generic functions (datatype|con|sel)Name
data Dmy (t :: Meta) (c :: Type -> Type) f = Dmy
