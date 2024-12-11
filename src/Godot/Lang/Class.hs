{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Godot.Lang.Class where

import Godot.Lang.Core
import Godot.Lang.Trans
import Data.Proxy (Proxy(Proxy))
-- import GHC.Generics (Generic(Rep), D1, (:+:), C1, (:*:), S1, Rec0, U1)
import GHC.Generics (Generic(Rep))
import Data.Typeable (Typeable)
import GHC.Base (Type)
import Control.Lens ((&), (%~))
import Godot.Lang.Functions (addBasicFunctions)
import Data.List (intercalate)
import Godot.Lang.Format (fmtDefCls)

-- | Class used for marking which type gets converted to corresponding godot type and adds additional functions and variables
class ToDC a where
   -- | DefCls type that corresponds with type 'a' with ToDC instance
   toDC :: Proxy a -> DefCls
   default toDC :: (GDC (Rep a), Typeable a) => Proxy a -> DefCls
   toDC _ = genDC @a

   extraStatVars :: Proxy a -> [DefVar]
   default extraStatVars :: Proxy a -> [DefVar]
   extraStatVars _ = []

   extraFuncs :: Proxy a -> [DefFunc]
   default extraFuncs :: Proxy a -> [DefFunc]
   extraFuncs _ = []

toDCExtra :: ToDC a => Proxy a -> DefCls
toDCExtra p = toDC p & dcInn . dciDefStatVars %~ (<> extraStatVars p)
                     & dcInn . dciDefFuncs %~ (<> extraFuncs p)


-- | Apply toDCExtra on multiple types
class ToDCsExtra (as :: [Type])                          where toDCsExtra :: Proxy as -> [DefCls]
instance                            ToDCsExtra '[]       where toDCsExtra _ = []
instance (ToDC a, ToDCsExtra as) => ToDCsExtra (a ': as) where toDCsExtra _ = toDCExtra (Proxy @a) : toDCsExtra (Proxy @as)

-- | Generate GD script of classes for a corresponging type list
toGDScriptExtra :: forall as. (ToDCsExtra as) => FilePath -> Proxy as -> IO ()
toGDScriptExtra dir _ =  writeFile (dir <> "/common.gd" ) $ intercalate "\n\n" $ fmtDefCls <$> dcs
    where
      dcs = addBasicFunctions <$> toDCsExtra (Proxy @as)

-- TODO: Below type families are still unused.
-- This will be used to recursivelly make DefCls conversion for types that are part of a type with ToDC instance
-- instead of adding ToDC instance manually to each type

-- | Type level list concatenation
-- type family LCat (as :: [k]) (bs :: [k]) :: [k] where
--   LCat '[] bs = bs
--   LCat (a ': as) bs = a ': LCat as bs

-- -- | Insert element at the end of list if element is not in the list
-- type family LIns (a :: k) (as :: [k]) :: [k] where
--   LIns a '[] = '[a]
--   LIns a (a ': as) = a ': as
--   LIns a (b ': as) = b ': LIns a as

-- -- | Union of two lists
-- type family LUnion (as :: [k]) (bs :: [k]) :: [k] where
--   LUnion '[] as = as
--   LUnion (a ': as) bs = LIns a (LUnion as bs)

-- -- | Get subtypes referenced in some type
-- type family SubTs (a :: Type) :: [Type] where
--   SubTs a = GSubTs (Rep a)

-- -- | Version of SubTs that operates on Generic representation
-- type family GSubTs (f :: Type -> Type) :: [Type] where
--   GSubTs (D1 m f) = GSubTs f
--   GSubTs (f :+: g) = LUnion (GSubTs f) (GSubTs g)
--   GSubTs (C1 m f) = GSubTs f
--   GSubTs (f :*: g) = LUnion (GSubTs f) (GSubTs g)
--   GSubTs (S1 m (Rec0 a)) = '[ a ]
--   GSubTs U1 = '[]
