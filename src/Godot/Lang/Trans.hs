{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


module Godot.Lang.Trans where

import Linear.V2(V2)
import Linear.V3(V3)
import Data.Kind (Type)
import GHC.TypeLits

import Data.Proxy
import Godot.Lang.Kind.General
import GHC.Generics (M1 (..), (:+:), (:*:), Generic (from, Rep), Meta (..), D, C, C1, S1, Rec0, U1)
import Control.Lens.TH(makeLenses)
import Control.Lens
import Data.Map.Strict (Map, insertWith, fromList, unionWith, toList)
import Control.Arrow ((>>>))
import Data.String.Interpolate (i)
import Data.List (intercalate)
import Godot.Lang.Core

-- Translation from Haskell type into Godot class

-- | Add specific constructor to Cons enum
addCons :: forall (con :: Symbol). KnownSymbol con => DefClsInn -> DefClsInn
addCons = addToEnum "Cons" (symbolVal (Proxy @con))



data CliMsg
  = JOIN
  | LEAVE Float
  | ACTION { act :: Action , integ :: Int}
  | BLA { xxx :: String}
  | GET_STATE
  deriving (Show, Generic)

data Action = MOVE Int
            | FIRE Int
  deriving (Show, Eq, Read)

-- genDefCls :: forall a. (MkC (Rep a)) => DefCls
-- genDefCls = mkC (Proxy @(Rep a))

-- | Build and value level DefCls from 'Generic' type representation
-- Handle multiple cases:
-- * For sum type make an exception if all constructors are 0-arry (no class, just simple enum)
-- * For singleton product type make an exception (no constructor enum, just regular product type)
-- class MkC a                                                                       where mkC :: Proxy a -> DefCls
-- instance (MkCSum f, KnownSymbol cls) => MkC (M1 D ('MetaData _m cls _fn _isnt) f) where mkC _ = mkCSum (emptyDefCls @cls) (Proxy @f)
-- -- instance MkCProd f => MkC (M1 i cn f) where mk _ = mkCProd (emptyDefCls ) $ Proxy @(f :*: g)

-- class MkCSum a                                                                                                                   where mkCSum :: DefCls -> Proxy a -> DefCls
-- instance (MkCProd f, MkCSum g, KnownSymbol con)  => MkCSum (C1 ('MetaCons con _fix 'True) f :+: g                              ) where mkCSum dc _ = mkCSum (addCons @con $ mkCProd dc (Proxy @f)) (Proxy @g)

-- instance (MkCSum f, MkCProd g, KnownSymbol con)  => MkCSum (f                               :+: C1 ('MetaCons con _fix 'True) g) where mkCSum dc _ = mkCProd (addCons @con $ mkCSum dc (Proxy @f)) (Proxy @g)
-- instance (MkCProd f, MkCProd g, KnownSymbol con) => MkCSum (C1 ('MetaCons con _fix 'True) f :+: C1 ('MetaCons con _fix 'True) g) where mkCSum dc _ = mkCProd (addCons @con $ mkCProd dc (Proxy @f)) (Proxy @g)

-- -- | Concat info from product type into regular class
-- class MkCProd a                                      where mkCProd :: DefCls -> Proxy a -> DefCls
-- instance (MkCField f, MkCProd g)  => MkCProd (S1 ('MetaSel ( 'Just field) su ss ds) f :*: g)                                       where mkCProd dc _ =  dc -- mkCSum Proxy
-- instance (MkCProd f,  MkCField g) => MkCProd (f                                       :*: S1 ('MetaSel ('Just field) su ss ds) g)  where mkCProd dc _ =  dc -- mkCSum Proxy
-- instance (MkCField f, MkCField g) => MkCProd (S1 ('MetaSel ('Just field) su  ss ds) f :*: S1 ('MetaSel ('Just field) su  ss ds) g) where mkCProd dc _ =  dc -- mkCSum Proxy


-- -- | Make a field for specific type
-- class MkCField a                                                                                                                                 where mkCField :: DefCls -> Proxy a -> DefCls
-- instance (MkCField f, MkCProd g)  => MkCField (S1 ('MetaSel ( 'Just field) su ss ds) (Rec0 f) :*: g)                                             where mkCField dc _ =  dc -- mkCSum Proxy
-- instance (MkCProd f,  MkCField g) => MkCField (f                                              :*: S1 ('MetaSel ('Just field) su ss ds) (Rec0 g)) where mkCField dc _ =  dc -- mkCSum Proxy
-- instance (MkCField f, MkCField g) => MkCField (S1 ('MetaSel ('Just field) su  ss ds) f :*: S1 ('MetaSel ('Just field) su  ss ds) g)              where mkCField dc _ =  dc -- mkCSum Proxy


genDefCls :: forall a. (GDC (Rep a)) => DefCls
genDefCls = gDC  (Proxy @(Rep a))


class GDC (f :: Type -> Type)                                                   where gDC :: Proxy f -> DefCls
instance (GDCISum f, KnownSymbol dat) => GDC (M1 D ('MetaData dat m fn isnt) f) where gDC _  = DefCls (ClassName $ symbolVal (Proxy @dat)) ExtendsObject $ gDCISum (Proxy @f) mempty

class GDCISum (f :: Type -> Type)                                                   where gDCISum :: Proxy f -> DefClsInn -> DefClsInn
instance (GDCISum f, GDCISum g)          => GDCISum (f :+: g)                       where gDCISum _ = gDCISum (Proxy @f) >>> gDCISum (Proxy @g)
instance (KnownSymbol con, GDCIProd f) => GDCISum (C1 ('MetaCons con fix hasRec) f) where gDCISum _ = addCons @con >>> gDCIProd (Proxy @f) (enumVal @con)

class GDCIProd (f :: Type -> Type)                                                            where gDCIProd :: Proxy f -> EnumVal -> DefClsInn -> DefClsInn
instance (GDCIProd f, GDCIProd g) => GDCIProd (f :*: g)                                       where gDCIProd _ con = gDCIProd (Proxy @f) con >>> gDCIProd (Proxy @g) con
instance (KnownSymbol field)      => GDCIProd (S1 ('MetaSel ('Just field) su ss ds) (Rec0 f)) where gDCIProd _ con = dciDefVars %~ (<>[Var $ "field_" <> evVal con <> "_" <> symbolVal (Proxy @field) ])
instance                             GDCIProd (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))      where gDCIProd _ con = dciDefVars %~ (<>[Var $ "field_" <> evVal con ])
instance                             GDCIProd U1                                              where gDCIProd _ _ = id



-- -- Template Haskell function to generate a string from a type-level symbol
-- generateString :: Q Exp
-- generateString = do
--   let str = symbolVal (Proxy :: Proxy (ToStrDefCls (MkDefCls CliMsg)))
--   [| str |]


