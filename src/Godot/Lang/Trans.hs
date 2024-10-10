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
import Godot.Lang.Kind.General

-- Helpers
--
-- | Max Width (max number of fields of any product)
class MW (f :: * -> *)                where mW :: Proxy f -> Int
instance (MW f, MW g) => MW (f :+: g) where mW _ = max (mW (Proxy @f)) (mW (Proxy @g))
instance (MW f, MW g) => MW (f :*: g) where mW _ = mW (Proxy @f) + mW (Proxy @g)
instance MW (K1 i c)                  where mW _ = 1
instance MW f => MW (M1 i t f)        where mW _ = mW (Proxy @f)
instance MW U1                        where mW _ = 0

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

-- | Generate GD script of classes for a corresponging type list
genGDScript' :: forall as. (GDCs (FmapRep as)) => FilePath -> IO ()
genGDScript' dir =  writeFile (dir <> "/common.gd" ) $ intercalate "\n\n" $ fmtDefCls <$> dcs
    where
      dcs = addBasicFunctions <$> genDefCls' @as

-- | Wrapper function. For a type with GCD instance on its representation, build DefCls generically
genDefCls :: forall a. (GDC (Rep a)) => DefCls
genDefCls = gDC $ Proxy @(Rep a)

type family FmapRep as where
  FmapRep '[]           = '[]
  FmapRep (a ': as) =  Rep a ': FmapRep as

-- | For a list of types with GCD instances on their representation, build DefCls' generically
genDefCls' :: forall as. (GDCs (FmapRep as)) => [DefCls]
genDefCls' = gDCs $ Proxy @(FmapRep as)

-- | Typeclass plural of GDC that works on a type level list
class GDCs (fs :: [Type -> Type])           where gDCs :: Proxy fs -> [DefCls]
instance                     GDCs '[]       where gDCs _ = []
instance (GDC f, GDCs fs) => GDCs (f ': fs) where gDCs _ = gDC (Proxy @f) : gDCs (Proxy @fs)

-- | Typeclass "GenericDefCls" whose instances (generic representations) know how to render themself into DefCls
class GDC (f :: Type -> Type)                                                 where gDC :: Proxy f -> DefCls
instance (GDCIΣ f, KnownSymbol dat) => GDC (M1 D ('MetaData dat m fn isnt) f) where gDC _  = DefCls (ClsName $ symbolVal (Proxy @dat)) ExtendsObject $ gDCIΣ (Proxy @f) mempty

-- | "GenericDefCls" logic on generic sum type
class GDCIΣ (f :: Type -> Type)                                                where gDCIΣ :: Proxy f -> DefClsInn -> DefClsInn
instance (GDCIΣ f, GDCIΣ g)         => GDCIΣ (f :+: g)                         where gDCIΣ _ = gDCIΣ (Proxy @f) >>> gDCIΣ (Proxy @g)
instance (KnownSymbol con, GDCIπ f) => GDCIΣ (C1 ('MetaCons con fix hasRec) f) where gDCIΣ _ = addCon @con >>> gDCIπ 0 (Proxy @f) (enumVal @con)

-- | "GenericDefCls" logic on generic product type
class GDCIπ (f :: Type -> Type)                                                            where gDCIπ :: Int -> Proxy f -> EnumVal -> DefClsInn -> DefClsInn
instance (GDCIπ f, GDCIπ g, MW f)   => GDCIπ (f :*: g)                                     where gDCIπ i _ = (>>>) <$> gDCIπ i (Proxy @f) <*> gDCIπ (i + mW (Proxy @f)) (Proxy @g)
instance (KnownSymbol fld, ToTyp f) => GDCIπ (S1 ('MetaSel ('Just fld) su ss ds) (Rec0 f)) where gDCIπ _ _ = addRecDefVar @fld (toTyp @f)
instance (ToTyp f)                  => GDCIπ (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))    where gDCIπ i _ = addConDefVar i (toTyp @f)
instance                               GDCIπ U1                                            where gDCIπ _ _ = const id

-- Generic represenation of a type name/label
--
-- | Wrapper function. For a type with GToTyp instance on its representation, show its type name/label
genToTyp :: forall a . (GToTyp (Rep a)) => Typ
genToTyp = gToTyp (Proxy @(Rep a))

-- | Typeclass whose instances (generic representations) know their type name/label
class GToTyp (f :: Type -> Type)                                                                    where gToTyp :: Proxy f -> Typ
instance {-# OVERLAPPABLE #-} (KnownSymbol dat, MW f) => GToTyp (M1 D ('MetaData dat m fn isnt) f)  where gToTyp _  = if mW (Proxy @f) == 0
                                                                                                                         then TypEnum  $ symbolVal (Proxy @dat)
                                                                                                                         else TypCls $ ClsName $ symbolVal (Proxy @dat)

-- | For any type with Generic instance, default to gToTyp as type name/label
instance {-# OVERLAPPABLE #-} GToTyp (Rep a) => ToTyp a where toTyp = genToTyp @a


