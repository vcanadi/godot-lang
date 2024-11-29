{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Godot.Lang.Trans where

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
import Godot.Lang.Kind.General
import Data.Typeable (typeRep, Typeable, TypeRep)

-- Helpers
--
-- | Max Width (max number of fields of any product)
class                    MW (f :: * -> *) where mW :: Proxy f -> Int
instance (MW f, MW g) => MW (f :+: g)     where mW _ = max (mW (Proxy @f)) (mW (Proxy @g))
instance (MW f, MW g) => MW (f :*: g)     where mW _ = mW (Proxy @f) + mW (Proxy @g)
instance                 MW (K1 i c)      where mW _ = 1
instance MW f         => MW (M1 i t f)    where mW _ = mW (Proxy @f)
instance                 MW U1            where mW _ = 0

-- | Dummy type used for GHC.Generic functions (datatype|con|sel)Name
data Dmy (t :: Meta) (c :: Type -> Type) f = Dmy

-- Translation from Haskell type into Godot class

-- | Add specific constructor to Con enum
addCon :: String -> DefClsInn -> DefClsInn
addCon  = addToConEnum

-- Generic generation of godot class from haskell type
--
-- | Generate GD script of a class for a corresponging type
genGDScript :: forall a. (GDC (Rep a), Typeable a) => FilePath -> IO ()
genGDScript dir =  writeFile (dir <> "/" <> cnName (_dcName dc) <> ".gd") $ fmtDefCls dc
    where
      dc = addBasicFunctions $ genDC @a

-- | Generate GD script of classes for a corresponging type list
genGDScript' :: forall as. (GenDCs as) => FilePath -> IO ()
genGDScript' dir =  writeFile (dir <> "/common.gd" ) $ intercalate "\n\n" $ fmtDefCls <$> dcs
    where
      dcs = addBasicFunctions <$> genDCs (Proxy @as)


type family FmapRep as where
  FmapRep '[]           = '[]
  FmapRep (a ': as) =  Rep a ': FmapRep as

-- | Apply genDC on multiple types
class GenDCs (as :: [Type])                                       where genDCs :: Proxy as -> [DefCls]
instance                                         GenDCs '[]       where genDCs _ = []
instance (GDC (Rep a), GenDCs as, Typeable a) => GenDCs (a ': as) where genDCs _ = genDC @a : genDCs (Proxy @as)

-- | Wrapper function to work on type 'a', not the representation.
-- For a type with GCD instance on its representation, build DefCls generically
genDC :: forall a. (GDC (Rep a), Typeable a) => DefCls
genDC = gDC (typeName @a) $ Proxy @(Rep a)

typeName :: forall a. Typeable a => String
typeName = concat $ words $ show $ typeRep (Proxy @a)

-- Work on generic representation

-- | Typeclass "GenericDefCls" whose instances (generic representations) know how to render themself into DefCls
class GDC (f :: Type -> Type)    where gDC :: String -> Proxy f -> DefCls
instance GDCIΣ f => GDC (D1 m f) where gDC tNm _  = DefCls (ClsName tNm) ExtendsObject $ gDCIΣ (Proxy @f) mempty

-- | "GenericDefCls" logic on generic sum type
class GDCIΣ (f :: Type -> Type)                      where gDCIΣ :: Proxy f -> DefClsInn -> DefClsInn
instance (GDCIΣ f, GDCIΣ g)       => GDCIΣ (f :+: g) where gDCIΣ _ = gDCIΣ (Proxy @f) >>> gDCIΣ (Proxy @g)
instance (GDCIπ f, Constructor m) => GDCIΣ (C1 m f)  where gDCIΣ _ = addToConEnum (conName @m Dmy) >>> gDCIπ 0 (Proxy @f) (EnumVal (conName @m Dmy))

-- | "GenericDefCls" logic on generic product type
class GDCIπ (f :: Type -> Type)                            where gDCIπ :: Int -> Proxy f -> EnumVal -> DefClsInn -> DefClsInn
instance (GDCIπ f, GDCIπ g, MW f) => GDCIπ (f :*: g)       where gDCIπ i _ = (>>>) <$> gDCIπ i (Proxy @f) <*> gDCIπ (i + mW (Proxy @f)) (Proxy @g)
instance (ToTyp f, Selector m)    => GDCIπ (S1 m (Rec0 f)) where gDCIπ i _ = case selName @m Dmy of "" -> addConDefVar i (toTyp @f); nm  -> addRecDefVar nm (toTyp @f)
instance                             GDCIπ U1              where gDCIπ _ _ = const id

-- Generic represenation of a type name/label
--
-- | Wrapper function. For a type with GToTyp instance on its representation, show its type name/label
genToTyp :: forall a . (GToTyp (Rep a)) => String -> Typ
genToTyp s = gToTyp s (Proxy @(Rep a))

-- | Typeclass whose instances (generic representations) know their type name/label
class GToTyp (f :: Type -> Type)                        where gToTyp :: String -> Proxy f -> Typ
instance {-# OVERLAPPABLE #-} (MW f) => GToTyp (D1 m f) where gToTyp tNm _  = (if mW (Proxy @f) == 0 then TypEnum else TypCls . ClsName) tNm

-- | For any type with Generic instance, default to gToTyp as type name/label
instance {-# OVERLAPPABLE #-} (GToTyp (Rep a), Typeable a) => ToTyp a where toTyp = genToTyp @a (typeName @a)


