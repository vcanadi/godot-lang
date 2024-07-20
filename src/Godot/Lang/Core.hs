{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}


module Godot.Lang.Core where

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
import qualified Data.Map.Strict as M
import Control.Arrow ((>>>))
import Data.String.Interpolate (i)
import Data.List (intercalate)

-- Godot language AST

-- | Name of the module
newtype ClsName = ClsName String deriving (Eq, Show, Semigroup, Monoid)

-- | Enum values
newtype EnumVal = EnumVal { evVal :: String } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Identifier
newtype VarName = VarName { vnName :: String } deriving (Eq, Show, Semigroup, Monoid)

-- | Call to some godot function
newtype FuncName = FuncName { cfFunc :: String } deriving (Eq, Show, Semigroup, Monoid)

-- | Generate enum val from type level string (symbol)
enumVal :: forall con. (KnownSymbol con) => EnumVal
enumVal = EnumVal $ symbolVal (Proxy @con)

-- | Inheritacne 'extends' expression
data Extends = ExtendsObject
             | ExtendsReference
  deriving (Eq,Show)

-- | Godot primitive type label (e.g. int, float, string)
data PrimTyp  where
  PTInt    :: PrimTyp
  PTFloat  :: PrimTyp
  PTString :: PrimTyp
  PTBool   :: PrimTyp
  PTV2     :: PrimTyp
  PTV3     :: PrimTyp
  PTArr    :: PrimTyp
  PTByteArr :: PrimTyp
deriving instance Eq (PrimTyp )
deriving instance Show (PrimTyp )

-- | Godot primitive value (e.g. 5, 4.0, "abc")
data PrimVal t where
  PVInt    :: Int       -> PrimVal Int
  PVFloat  :: Double    -> PrimVal Double
  PVString :: String    -> PrimVal String
  PVBool   :: Bool      -> PrimVal Bool
  PVV2     :: V2 Double -> PrimVal (V2 Double)
  PVV3     :: V3 Double -> PrimVal (V3 Double)
deriving instance Eq (PrimVal t)
deriving instance Show (PrimVal t)

-- | Godot class type label (e.g.
data ClsTyp = ClsTyp deriving (Eq,Show)

-- | Godot class value (list of optionally set fields)
newtype ClsVal = ClsVal { cvVars :: [DefVar ] }
deriving instance Eq ClsVal
deriving instance Show ClsVal

-- | Godot array type label
data ArrTyp = ArrTyp deriving (Eq,Show)

-- | Godot array vlue
newtype ArrVal a = ArrVal [a] deriving (Eq,Show)

-- | Any godot type (primitives + custom classes + arrays)
data Typ where
  TypPrim :: PrimTyp -> Typ
  TypCls :: ClsName -> Typ
  TypArr :: ArrVal Typ  -> Typ

deriving instance Eq Typ
deriving instance Show Typ

data Val t where
  ValPrim :: PrimVal t -> Val t
  ValCls :: ClsVal -> Val ClsTyp

-- | Main type representing GD script AST
newtype Script = Script
  { gdsClass :: DefCls
  }

-- | Variable declaration
data DefVar = DefVar
  { varName :: VarName
  , varType :: Typ
  } deriving (Eq,Show)

data DefFunc = DefFunc
  { _dfIsStat :: Bool
  , _dfName :: FuncName
  , _dfArgs :: [DefVar]
  , _dfOutTyp :: Typ
  , _dfLocalVars :: [DefVar]
  , _dfStmts :: forall t. [Stmt t]
  }
deriving instance Show DefFunc

data Stmt t where
  StmtCallFunc :: FuncName -> Stmt t
  StmtIf :: Expr Bool -> Stmt t -> Stmt t
  StmtIfElse :: Expr Bool -> Stmt t -> Stmt t -> Stmt t
  StmtFor :: VarName -> (Expr Enumerable) -> Stmt t -> Stmt t
  StmtMatch :: Expr t -> [(Expr t, Stmt r)] -> Stmt r
  StmtRet :: Expr t -> Stmt r
deriving instance Show (Stmt t)

-- | Type used for flaging godot expression acceptable for for loop
data Enumerable = Enumerable

-- | Type used for flaging godot raw expression
data Raw = Raw

data Expr t where
  ExprFalse :: Expr Bool
  ExprTrue :: Expr Bool
  ExprRange :: Int -> Int -> Int -> Expr Enumerable
  ExprRangeVar :: VarName -> Expr Enumerable
  ExprStr :: String -> Expr Raw
  ExprRaw :: String -> Expr Raw

deriving instance Eq (Expr t)
deriving instance Show (Expr t)

-- | Main type describing godot class
data DefClsInn = DefClsInn
  { _dciDefEnums :: Map String [EnumVal] -- ^ Local class enums
  , _dciDefConEnum :: [EnumVal] -- ^ Special enum representing constructor
  , _dciDefClasses :: [DefCls]
  , _dciDefConsts :: [DefVar]
  , _dciDefConVars :: Map EnumVal [DefVar] -- ^ Godot's variables indexed with a constructor
  , _dciDefVars :: [DefVar]
  , _dciDefFuncs :: [DefFunc]
  }

deriving instance Show DefClsInn

-- | Defintion of a godot type/class
data DefCls = DefCls
  { _dcName :: ClsName
  , _dcExtends :: Extends
  , _dcInn :: DefClsInn
  } deriving (Show)

$(makeLenses ''DefClsInn)
$(makeLenses ''DefCls)


-- Combinators for building DefCls

emptyDefCls :: forall cls. (KnownSymbol cls) => DefCls
emptyDefCls  = DefCls (ClsName $ symbolVal (Proxy @cls)) ExtendsObject $ emptyDefClsInn

emptyDefClsInn ::  DefClsInn
emptyDefClsInn  = DefClsInn mempty [] [] [] mempty [] []


-- | Join two DefClsInns by making a union of enums and union of its fields
joinDefClsInn :: DefClsInn -> DefClsInn -> DefClsInn
joinDefClsInn dci0 dci1 =
  DefClsInn
    (unionWith (<>) (_dciDefEnums dci0) (_dciDefEnums dci1))
    (_dciDefConEnum dci0 <> _dciDefConEnum dci1)
    (_dciDefClasses dci0 <> _dciDefClasses dci1)
    (_dciDefConsts dci0 <> _dciDefConsts dci1)
    (unionWith (<>) (_dciDefConVars dci0) (_dciDefConVars dci1))
    (_dciDefVars dci0 <> _dciDefVars dci1)
    (_dciDefFuncs dci0 <> _dciDefFuncs dci1)


instance Semigroup DefClsInn where (<>) = joinDefClsInn
instance Monoid DefClsInn where mempty =  emptyDefClsInn

addToEnum :: String -> String -> DefClsInn -> DefClsInn
addToEnum k v = dciDefEnums %~ insertWith (flip (<>)) k [EnumVal v]

addToConEnum :: String -> DefClsInn -> DefClsInn
addToConEnum v = dciDefConEnum %~ (<> [EnumVal v])

addConDefVar :: forall field typ. (KnownSymbol field, ToTyp typ) => EnumVal -> DefClsInn -> DefClsInn
addConDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [DefVar (VarName $ "field_" <> evVal con <> "_" <> symbolVal (Proxy @field)) (toTyp @typ) ]

addUnConDefVar :: forall typ. ToTyp typ => EnumVal -> DefClsInn -> DefClsInn
addUnConDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [DefVar (VarName $ "field_" <> evVal con) (toTyp @typ) ]

addDefFunc :: DefFunc -> DefCls -> DefCls
addDefFunc f dc = dc & over (dcInn . dciDefFuncs) (<> [f])

addDefFuncs :: [DefFunc] -> DefCls -> DefCls
addDefFuncs fs dc = dc & over (dcInn . dciDefFuncs) (<> fs)

addBasicFunctions :: DefCls -> DefCls
addBasicFunctions = addSerialization
                  . addConShow

-- | Enrich DefCls with "con" enum with show function
addConShow :: DefCls -> DefCls
addConShow dc = (`addDefFunc` dc) $
  DefFunc False (FuncName "show") [] (TypPrim PTString) []
        [ StmtMatch (ExprRaw "self.con")
           [(ExprRaw $ "Con." <> en , StmtRet (ExprStr en)) | EnumVal en <- _dciDefConEnum $ _dcInn dc]
        ]

-- | Enrich DefCls with serialization function compatible with godot-ser serialization
addSerialization :: DefCls -> DefCls
addSerialization dc = (`addDefFuncs` dc)
  [ DefFunc True (FuncName "serArr") [DefVar (VarName "this") $ TypCls $ _dcName dc] (TypPrim PTArr) []
      [
      ]
  , DefFunc True (FuncName "ser") [DefVar (VarName "this") $ TypCls $ _dcName dc] (TypPrim PTByteArr) []
      [StmtRet $ ExprRaw "var_to_bytes(serArr(this))"]
  ]


  -- static func ser(m: CliMsg) -> PackedByteArray: return var_to_bytes(serArr(m))

class ToTyp t where
  toTyp :: Typ

instance ToTyp Int where toTyp = TypPrim PTInt
instance ToTyp Double where toTyp = TypPrim PTFloat
instance ToTyp Float where toTyp = TypPrim PTFloat
instance ToTyp String where toTyp = TypPrim PTString
instance ToTyp (V2 n) where toTyp = TypPrim PTV2

