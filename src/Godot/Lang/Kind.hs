{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Godot.Lang.Kind where

import Linear.V2(V2)
import Linear.V3(V3)
import Data.Kind (Type)
import GHC.TypeLits

import Data.Proxy
import Godot.Lang.Kind.General
import GHC.Generics (M1 (..), (:+:), (:*:), Generic (from, Rep), Meta (..), D, C, C1, S1, Rec0, U1)
import Control.Lens.TH(makeLenses)
import Control.Lens
import Data.Map.Strict (Map, insertWith, fromList, unionWith)

-- | Godot language/script AST
-- Here are defined types which are lifted into kinds and whose values (as types) build godot's AST during compilation

-- | Name of the module
type ClassName = String

-- | Inheritacne 'extends' expression
data Extends = ExtendsObject
             | ExtendsReference
  deriving (Eq,Show)

-- | Godot primitive types
data Prim t where
  PrimInt    :: Int       -> Prim Int
  PrimFloat  :: Double    -> Prim Double
  PrimString :: String    -> Prim String
  PrimBool   :: Bool      -> Prim Bool
  PrimV2     :: V2 Double -> Prim (V2 Double)
  PrimV3     :: V3 Double -> Prim (V3 Double)

-- | Any godot type (primitives + custom classes)
data Typ t where
  TypNat :: Prim t -> Typ (Prim t)
  TypClass :: DefCls -> Typ DefCls

-- | Main type representing GD script AST
newtype Script = Script
  { gdsClass :: DefCls
  }

-- | Variable declaration
data Var = Var
  { varName :: String
  -- , defVarValue :: GDType
  } deriving (Eq,Show)

data DefClsInn = DefClsInn
  { _dciExtends :: Extends
  , _dciDefEnums :: Map String [String]
  , _dciDefClasses :: [DefCls]
  , _dciDefConsts :: [Var]
  , _dciDefVars :: [Var]
  -- , dciFuncs :: [DefFunc]
  } deriving (Eq,Show)

-- | Defintion of a godot type/class
data DefCls = DefCls
  { _dcName :: ClassName
  , _dcInn :: DefClsInn
  } deriving (Eq,Show)

$(makeLenses ''DefClsInn)
$(makeLenses ''DefCls)


-- Combinators for building DefCls

emptyDefCls :: forall cls. (KnownSymbol cls) => DefCls
emptyDefCls  = DefCls (symbolVal (Proxy @cls)) $ emptyDefClsInn

emptyDefClsInn ::  DefClsInn
emptyDefClsInn  = DefClsInn ExtendsObject mempty [] [] []


-- | Join two DefClsInns by making a union of cons enum and union of its fields
joinDefClsInn :: DefClsInn -> DefClsInn -> DefClsInn
joinDefClsInn dci0 dci1 = DefClsInn ExtendsObject (unionWith (<>) (_dciDefEnums dci0) (_dciDefEnums dci1)) [] [] (_dciDefVars dci0 <> _dciDefVars dci1)

instance Semigroup DefClsInn where (<>) = joinDefClsInn
instance Monoid DefClsInn where mempty =  emptyDefClsInn

addToEnum :: String -> String -> DefClsInn -> DefClsInn
addToEnum k v = dciDefEnums %~ insertWith (<>) k [v]

-- | Add specific constructor to cons enum
singletonCon :: forall (con :: Symbol). KnownSymbol con => DefClsInn
singletonCon = addToEnum "cons" (symbolVal (Proxy @con)) emptyDefClsInn

-- | Add specific constructor to cons enum
addCons :: forall (con :: Symbol). KnownSymbol con => DefClsInn -> DefClsInn
addCons = addToEnum "cons" (symbolVal (Proxy @con))



-- | Defintion of a godot function
-- data Func a b = Func
--   { defFuncArgs :: [Vars]
--   , defFuncLocals :: [Vars]
--   , defFuncStmts :: [Stms]
--   }

-- | GD script/language statement
-- data Stmt
--   = If GDBool Stmt (Maybe Stmt)
--   -- TODO: implement this

-- class GDType t where
--   GDType


-- instance AsText DefVar where
--   asText (DefVar nm val) = show nm "=" val

data CliMsg
  = JOIN
  | LEAVE Float
  | ACTION { act :: Action , integ :: Int}
  | GET_STATE
  deriving (Show, Generic)

data Action = MOVE Int
            | FIRE Int
  deriving (Show, Eq, Read)


class MkDefCls a where
  defCls :: Proxy a -> DefCls

instance MkDefCls CliMsg where
  defCls Proxy = DefCls "CliMsg" $ DefClsInn
    ExtendsObject
    (fromList [  ("CliMsgCon", [ "JOIN", "LEAVE", "ACTION", "GET_STATE" ]) ])
    []
    []
    []


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


class GDC a                                                                     where gDC :: Proxy a -> DefCls
instance (GDCI f, KnownSymbol dat) => GDC (M1 D ('MetaData dat m fn isnt) f) where gDC _  = DefCls (symbolVal (Proxy @dat)) $ gDCI (Proxy @f)

class GDCI a                                                                               where gDCI :: Proxy a -> DefClsInn
instance (GDCI f, GDCI g)          => GDCI (f :+: g)                                       where gDCI _ = gDCI (Proxy @f) <> gDCI (Proxy @g)
instance (KnownSymbol con, GDCI f) => GDCI (C1 ('MetaCons con fix hasRec) f)               where gDCI _ = singletonCon @con <> gDCI (Proxy @f)
instance (GDCI f, GDCI g)          => GDCI (f :*: g)                                       where gDCI _ = gDCI (Proxy @f) <> gDCI (Proxy @g)
instance (KnownSymbol field)       => GDCI (S1 ('MetaSel ('Just field) su ss ds) (Rec0 f)) where gDCI _ = emptyDefClsInn & dciDefVars %~ (<>[Var $ symbolVal (Proxy @field) ])
instance                              GDCI (S1 ('MetaSel 'Nothing su ss ds) (Rec0 f))      where gDCI _ = emptyDefClsInn & dciDefVars %~ (<>[Var $ symbolVal (Proxy @"XX") ])
instance                              GDCI U1                                              where gDCI _ = emptyDefClsInn







-- class MkC a                                                                                      where mkC :: DefCls -> Proxy a -> DefCls
-- instance (MkC f, KnownSymbol dat)        => MkC (M1 D ('MetaData _m dat _fn _isnt) f)            where mkC _ _  = mkC (emptyDefCls @dat) (Proxy @f)
-- instance (KnownSymbol con)               => MkC (C1 ('MetaCons con _fix hasRec) f)               where mkC dc _ = dc
-- instance (MkC f, MkC g, KnownSymbol con) => MkC (C1 ('MetaCons con _fix hasRec) f :+: g)         where mkC dc _ = mkC (addCons @con $ mkC dc (Proxy @f)) (Proxy @g)
-- instance (MkC f, MkC g, KnownSymbol con) => MkC (f :+: C1 ('MetaCons con _fix hasRec) g)         where mkC dc _ = mkC (addCons @con $ mkC dc (Proxy @f)) (Proxy @g)
-- instance (MkC f, MkC g)                  => MkC (f :*: g)                                        where mkC dc _ =  dc -- mkCSum Proxy
-- instance                                    MkC (S1 ('MetaSel ( 'Just field) su ss ds) (Rec0 f)) where mkC dc _ =  dc -- mkCSum Proxy































-- instance (Serializable a) => SR (K1 x a) where srG (K1 v) = ser v
--
-- instance SR U1                           where srG U1 = ""


-- | Represent a type as a godot Class
-- class ToCls a where
--   toCls :: DefCls

-- instance ToCls CliMsg where
--   toCls = DefCls "CliMsg" ExtendsObject (DefEnm "SrvMsgCon" [])

-- type family MkDefCls t :: DefCls

-- type instance MkDefCls CliMsg =
--   'DefCls
--     "CliMsg"
--     'ExtendsObject
--     '[ 'DefEnm "CliMsgCon" '[ "JOIN", "LEAVE", "ACTION", "GET_STATE" ] ]
--     '[]
--     '[]
--     '[]

-- type family ToStrExtends (t :: Extends) :: Symbol where
--   ToStrExtends 'ExtendsObject = "extends object"
--   ToStrExtends 'ExtendsReference = "extends reference"

-- type family ToStrDefEnms (as :: [DefEnm]) = (b :: Symbol) where
--   ToStrDefEnms '[] = ""
--   ToStrDefEnms (a ': as) = AppendSymbol (ToStrDefEnm a) (ToStrDefEnms as)

-- type family ToStrDefEnm (t :: DefEnm) = (r :: Symbol) where
--   ToStrDefEnm ('DefEnm nm vals) = AppendSymbol "enum" (ToStrDefEnmVals vals)

-- type family ToStrDefEnmVals (as :: [Symbol]) = (b :: Symbol) where
--   ToStrDefEnmVals '[] = ""
--   ToStrDefEnmVals (a ': as) = AppendSymbol a (ToStrDefEnmVals as)


-- type family ToStrDefCls (t :: DefCls) :: Symbol where
--   ToStrDefCls ( 'DefCls nm ext ens clss consts vars ) =
--      ("class_name " `AppendSymbol` nm `AppendSymbol` "\n\n"
--        `AppendSymbol` ToStrExtends ext `AppendSymbol` "\n"
--        `AppendSymbol` ToStrDefEnms ens `AppendSymbol` "\n"
--      )



-- -- Template Haskell function to generate a string from a type-level symbol
-- generateString :: Q Exp
-- generateString = do
--   let str = symbolVal (Proxy :: Proxy (ToStrDefCls (MkDefCls CliMsg)))
--   [| str |]


