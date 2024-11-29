{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Godot.Lang.Util where

import Linear.V2(V2)
import Linear.V3(V3)
import Data.Kind (Type)
import GHC.TypeLits

import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (from, Rep), Meta (..), D, C, C1, S1, Rec0, U1, K1, D1, Datatype (datatypeName), Constructor (conName), Selector (selName))
import Control.Lens.TH(makeLenses)
import Control.Lens
import Data.Map.Strict (Map, insertWith, fromList, unionWith, toList)
import Control.Arrow ((>>>))
import Data.String.Interpolate (i)
import Data.List (intercalate)
import Godot.Lang.Core
import Godot.Lang.Functions
import Godot.Lang.Format
import Language.Haskell.TH (Q, Exp, runIO)
import Data.Typeable (typeRep, Typeable, TypeRep)

-- | Max Width (max number of fields of any product)
class                    MW (f :: * -> *) where mW :: Proxy f -> Int
instance (MW f, MW g) => MW (f :+: g)     where mW _ = max (mW (Proxy @f)) (mW (Proxy @g))
instance (MW f, MW g) => MW (f :*: g)     where mW _ = mW (Proxy @f) + mW (Proxy @g)
instance                 MW (K1 i c)      where mW _ = 1
instance MW f         => MW (M1 i t f)    where mW _ = mW (Proxy @f)
instance                 MW U1            where mW _ = 0

-- | Dummy type used for GHC.Generic functions (datatype|con|sel)Name
data Dmy (t :: Meta) (c :: Type -> Type) f = Dmy
