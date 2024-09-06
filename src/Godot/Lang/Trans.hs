{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Godot.Lang.Trans where

import Linear.V2(V2)
import Linear.V3(V3)
import Data.Kind (Type)
import GHC.TypeLits

import Data.Proxy
import Godot.Lang.Kind.General
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (from, Rep), Meta (..), D, C, C1, S1, Rec0, U1, K1)
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

-- Helpers
--
-- | "Horizontal" max size (max number of fields of any product)
class MSzH (f :: * -> *)           where mSzH :: Proxy f -> Int
instance (MSzH f, MSzH g)
         => MSzH (f :+: g)         where mSzH _ = max (mSzH (Proxy @f)) (mSzH (Proxy @g))
instance (MSzH f, MSzH g)
         => MSzH (f :*: g)         where mSzH _ = mSzH (Proxy @f) + mSzH (Proxy @g)
instance MSzH (K1 i c)             where mSzH _ = 1
instance MSzH f => MSzH (M1 i t f) where mSzH _ = mSzH (Proxy @f)
instance MSzH U1                   where mSzH _ = 0

-- Translation from Haskell type into Godot class

-- | Add specific constructor to Con enum
addCon :: forall (con :: Symbol). KnownSymbol con => DefClsInn -> DefClsInn
addCon = addToConEnum (symbolVal (Proxy @con))

-- Generic generation of godot class from haskell type
--
-- | Generate GD script of a class for a corresponging type
genGDScript :: forall a. (GDC (Rep a)) => FilePath -> IO ()
genGDScript dir =  writeFile (dir <> "/" <> cnName (_dcName dc) <> ".gd") $ fmtDefCls dc
    where
      dc = addBasicFunctions $ genDefCls @a

-- | Wrapper function. For a type with GCD instance on its representation, build DefCls generically
genDefCls :: forall a. (GDC (Rep a)) => DefCls
genDefCls = gDC  (Proxy @(Rep a))

-- | Typeclass "GenericDefCls" whose instances (generic representations) know how to render themself into DefCls
class GDC (f :: Type -> Type)                                                 where gDC :: Proxy f -> DefCls
instance (GDCIΣ f, KnownSymbol dat) => GDC (M1 D ('MetaData dat m fn isnt) f) where gDC _  = DefCls (ClsName $ symbolVal (Proxy @dat)) ExtendsObject $ gDCIΣ (Proxy @f) mempty

-- | "GenericDefCls" logic on generic sum type
class GDCIΣ (f :: Type -> Type)                                                where gDCIΣ :: Proxy f -> DefClsInn -> DefClsInn
instance (GDCIΣ f, GDCIΣ g)         => GDCIΣ (f :+: g)                         where gDCIΣ _ = gDCIΣ (Proxy @f) >>> gDCIΣ (Proxy @g)
instance (KnownSymbol con, GDCIπ f) => GDCIΣ (C1 ('MetaCons con fix hasRec) f) where gDCIΣ _ = addCon @con >>> gDCIπ 0 (Proxy @f) (enumVal @con)

-- | "GenericDefCls" logic on generic product type
class GDCIπ (f :: Type -> Type)                                                            where gDCIπ :: Int -> Proxy f -> EnumVal -> DefClsInn -> DefClsInn
instance (GDCIπ f, GDCIπ g, MSzH f) => GDCIπ (f :*: g)                                     where gDCIπ i _ = (>>>) <$> gDCIπ i (Proxy @f) <*> gDCIπ (i + mSzH (Proxy @f)) (Proxy @g)
instance (KnownSymbol fld, ToTyp f) => GDCIπ (S1 ('MetaSel ('Just fld) su ss ds) (Rec0 f)) where gDCIπ _ _ = addRecDefVar @fld @f
instance (ToTyp f)                  => GDCIπ (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))    where gDCIπ i _ = addConDefVar @f i
instance                               GDCIπ U1                                            where gDCIπ _ _ = const id

-- Generic represenation of a type name/label
--
--
-- | Wrapper function. For a type with GToTyp instance on its representation, show its type name/label
genToTyp :: forall a . (GToTyp (Rep a)) => Typ
genToTyp = gToTyp (Proxy @(Rep a))

-- | Typeclass whose instances (generic representations) know their type name/label
class GToTyp (f :: Type -> Type)                                                              where gToTyp :: Proxy f -> Typ
instance {-# OVERLAPPABLE #-} (KnownSymbol dat) => GToTyp (M1 D ('MetaData dat m fn isnt) f)  where gToTyp _  = TypCls $ ClsName $ symbolVal (Proxy @dat)
-- instance {-# OVERLAPS #-}                          GToTyp (M1 D ('MetaData "[]" m fn isnt) f) where gToTyp _  = TypArr

-- | For any type with Generic instance, default to gToTyp as type name/label
instance {-# OVERLAPPABLE #-} GToTyp (Rep a) => ToTyp a where toTyp = genToTyp @a


