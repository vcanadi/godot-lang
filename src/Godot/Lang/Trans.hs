{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}


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
import Godot.Lang.Format
import Language.Haskell.TH (Q, Exp, runIO)

-- Translation from Haskell type into Godot class

-- | Add specific constructor to Con enum
addCon :: forall (con :: Symbol). KnownSymbol con => DefClsInn -> DefClsInn
addCon = addToConEnum (symbolVal (Proxy @con))

data CliMsg
  = JOIN
  | LEAVE String
  | ACTION { act :: Action , time :: Int, vec :: V2 Double}
  | GET_LISTS { glList :: [Float], glList2 :: [Action] }
  | GET_MAP { gmMap :: Map Float String}
  deriving (Show, Generic)

data Action = MOVE Int
            | FIRE Int
  deriving (Show, Eq, Read, Generic)

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
class GDC (f :: Type -> Type)                                                   where gDC :: Proxy f -> DefCls
instance (GDCISum f, KnownSymbol dat) => GDC (M1 D ('MetaData dat m fn isnt) f) where gDC _  = DefCls (ClsName $ symbolVal (Proxy @dat)) ExtendsObject $ gDCISum (Proxy @f) mempty

-- | "GenericDefCls" logic on generic sum type
class GDCISum (f :: Type -> Type)                                                   where gDCISum :: Proxy f -> DefClsInn -> DefClsInn
instance (GDCISum f, GDCISum g)        => GDCISum (f :+: g)                         where gDCISum _ = gDCISum (Proxy @f) >>> gDCISum (Proxy @g)
instance (KnownSymbol con, GDCIProd f) => GDCISum (C1 ('MetaCons con fix hasRec) f) where gDCISum _ = addCon @con >>> gDCIProd (Proxy @f) (enumVal @con)

-- | "GenericDefCls" logic on generic product type
class GDCIProd (f :: Type -> Type)                                                                where gDCIProd :: Proxy f -> EnumVal -> DefClsInn -> DefClsInn
instance (GDCIProd f, GDCIProd g)     => GDCIProd (f :*: g)                                       where gDCIProd _ = (>>>) <$> gDCIProd (Proxy @f) <*> gDCIProd (Proxy @g)
instance (KnownSymbol field, ToTyp f) => GDCIProd (S1 ('MetaSel ('Just field) su ss ds) (Rec0 f)) where gDCIProd _ = addRecConDefVar @field @f
instance (ToTyp f)                    => GDCIProd (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))      where gDCIProd _ = addConDefVar @f
instance                                 GDCIProd U1                                              where gDCIProd _ = const id

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

generateGDScript :: Q Exp
generateGDScript = do
    runIO $ genGDScript @CliMsg "./gd-autogen"
    [| "This string is gen erated at compile-time." |]
