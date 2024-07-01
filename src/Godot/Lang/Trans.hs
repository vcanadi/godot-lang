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
  | NEWCON { vec :: V2 Double}
  | GET_STATE
  deriving (Show, Generic)

data Action = MOVE Int
            | FIRE Int
  deriving (Show, Eq, Read, Generic)

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




-- Generic generation of godot class from haskell type
--
-- | Wrapper function. For a type with GCD instance on its representation, build DefCls generically
genDefCls :: forall a. (GDC (Rep a)) => DefCls
genDefCls = gDC  (Proxy @(Rep a))

-- | Typeclass "GenericDefCls" whose instances (generic representations) know how to render themself into DefCls
class GDC (f :: Type -> Type)                                                   where gDC :: Proxy f -> DefCls
instance (GDCISum f, KnownSymbol dat) => GDC (M1 D ('MetaData dat m fn isnt) f) where gDC _  = DefCls (ClsName $ symbolVal (Proxy @dat)) ExtendsObject $ gDCISum (Proxy @f) mempty

-- | "GenericDefCls" logic on generic sum type
class GDCISum (f :: Type -> Type)                                                   where gDCISum :: Proxy f -> DefClsInn -> DefClsInn
instance (GDCISum f, GDCISum g)          => GDCISum (f :+: g)                       where gDCISum _ = gDCISum (Proxy @f) >>> gDCISum (Proxy @g)
instance (KnownSymbol con, GDCIProd f) => GDCISum (C1 ('MetaCons con fix hasRec) f) where gDCISum _ = addCons @con >>> gDCIProd (Proxy @f) (enumVal @con)

-- | "GenericDefCls" logic on generic product type
class GDCIProd (f :: Type -> Type)                                                                where gDCIProd :: Proxy f -> EnumVal -> DefClsInn -> DefClsInn
instance (GDCIProd f, GDCIProd g)     => GDCIProd (f :*: g)                                       where gDCIProd _ con = gDCIProd (Proxy @f) con >>> gDCIProd (Proxy @g) con
instance (KnownSymbol field, ToTyp f) => GDCIProd (S1 ('MetaSel ('Just field) su ss ds) (Rec0 f)) where gDCIProd _ con = dciDefVars %~ (<>[DefVar (VarName $ "field_" <> evVal con <> "_" <> symbolVal (Proxy @field)) (Just $ toTyp @f) ])
instance (ToTyp f)                    => GDCIProd (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))      where gDCIProd _ con = dciDefVars %~ (<>[DefVar (VarName $ "field_" <> evVal con) (Just $ toTyp @f) ])
instance                                 GDCIProd U1                                              where gDCIProd _ _ = id

-- Generic represenation of a type name/label
--
--
-- | Wrapper function. For a type with GToTyp instance on its representation, show its type name/label
genToTyp :: forall a . (GToTyp (Rep a)) => Typ
genToTyp = gToTyp (Proxy @(Rep a))

-- | Typeclass whose instances (generic representations) know their type name/label
class GToTyp (f :: Type -> Type)                                        where gToTyp :: Proxy f -> Typ
instance (KnownSymbol dat) => GToTyp (M1 D ('MetaData dat m fn isnt) f) where gToTyp _  = TypCls $ ClsName $ symbolVal (Proxy @dat)

-- | For any type with Generic instance, default to gToType as type name/label
instance {-# OVERLAPPABLE #-} GToTyp (Rep a) => ToTyp a where toTyp = genToTyp @a

--
-- genTypLbl :: forall a. (GTL (Rep a)) => String
-- genTypLbl = gTL  (Proxy @(Rep a))

-- | Generic generation of godot type label for haskell type
--
-- class GTL (f :: Type -> Type)                                                   where gTL :: Proxy f -> String
-- instance (KnownSymbol dat) => GTL (M1 D ('MetaData dat m fn isnt) f) where gTL _  = symbolVal (Proxy @dat)

-- class GTLISum (f :: Type -> Type)                                                   where gTLISum :: Proxy f -> DefClsInn -> DefClsInn
-- instance (GTLISum f, GTLISum g)          => GTLISum (f :+: g)                       where gTLISum _ = gTLISum (Proxy @f) >>> gTLISum (Proxy @g)
-- instance (KnownSymbol con, GTLIProd f) => GTLISum (C1 ('MetaCons con fix hasRec) f) where gTLISum _ = addCons @con >>> gTLIProd (Proxy @f) (enumVal @con)

-- class GTLIProd (f :: Type -> Type)                                                            where gTLIProd :: Proxy f -> EnumVal -> DefClsInn -> DefClsInn
-- instance (GTLIProd f, GTLIProd g) => GTLIProd (f :*: g)                                       where gTLIProd _ con = gTLIProd (Proxy @f) con >>> gTLIProd (Proxy @g) con
-- instance (KnownSymbol field)      => GTLIProd (S1 ('MetaSel ('Just field) su ss ds) (Rec0 f)) where gTLIProd _ con = dciDefVars %~ (<>[DefVar (VarName $ "field_" <> evVal con <> "_" <> symbolVal (Proxy @field)) Nothing ])
-- instance                             GTLIProd (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))      where gTLIProd _ con = dciDefVars %~ (<>[DefVar (VarName $ "field_" <> evVal con) Nothing ])
-- instance                             GTLIProd U1                                              where gTLIProd _ _ = id



-- -- Template Haskell function to generate a string from a type-level symbol
-- generateString :: Q Exp
-- generateString = do
--   let str = symbolVal (Proxy :: Proxy (ToStrDefCls (MkDefCls CliMsg)))
--   [| str |]


