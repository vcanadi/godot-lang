{-# LANGUAGE DefaultSignatures #-}

module Godot.Lang.Class where

import Godot.Lang.Core
import Godot.Lang.Trans
import Data.Proxy (Proxy)
import GHC.Generics (Generic(Rep))
import Data.Typeable (Typeable)

-- | Class used for flagging which type gets converted to corresponding godot type
class ToDefCls a where
   toDC :: Proxy a -> DefCls
   default toDC :: (GDC (Rep a), Typeable a) => Proxy a -> DefCls
   toDC _ = genDC @a

