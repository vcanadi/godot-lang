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
import Control.Arrow ((>>>))
import Data.String.Interpolate (i)
import Data.List (intercalate)

-- Godot language AST

-- | Name of the module
newtype ClsName = ClsName String deriving (Eq, Show, Semigroup, Monoid)

-- | Enum values
newtype EnumVal = EnumVal { evVal :: String } deriving (Eq, Show, Semigroup, Monoid)

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
data ClsTyp deriving (Eq,Show)

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
  , varType :: Maybe Typ
  } deriving (Eq,Show)

data DefFunc = DefFunc
  { _dfIsStat :: Bool
  , _dfName :: FuncName
  , _dfArgs :: forall t. [DefVar]
  , _dfLocalVars :: forall t. [DefVar]
  , _dfStmts :: [Stmt]
  }

deriving instance Eq DefFunc
deriving instance Show DefFunc

data Stmt
  = StmtCallFunc FuncName
  | StmtIf (Expr Bool) Stmt
  | StmtIfElse (Expr Bool) Stmt (Expr Bool) Stmt
  | StmtFor VarName (Expr Enumerable) Stmt
   deriving (Eq,Show)

-- | Type used for flaging godot expression acceptable for for loop
data Enumerable = Enumerable

data Expr t where
  ExprFalse :: Expr Bool
  ExprTrue :: Expr Bool
  ExprRange :: Int -> Int -> Int -> Expr Enumerable
  ExprRangeVar :: VarName -> Expr Enumerable

deriving instance Eq (Expr t)
deriving instance Show (Expr t)

data DefClsInn = DefClsInn
  { _dciDefEnums :: Map String [EnumVal]
  , _dciDefClasses :: [DefCls]
  , _dciDefConsts :: [DefVar]
  , _dciDefVars :: [DefVar]
  , _dciDefFuncs :: [DefFunc]
  }

deriving instance Eq DefClsInn
deriving instance Show DefClsInn

-- | Defintion of a godot type/class
data DefCls = DefCls
  { _dcName :: ClsName
  , _dcExtends :: Extends
  , _dcInn :: DefClsInn
  } deriving (Eq,Show)

$(makeLenses ''DefClsInn)
$(makeLenses ''DefCls)


-- Combinators for building DefCls

emptyDefCls :: forall cls. (KnownSymbol cls) => DefCls
emptyDefCls  = DefCls (ClsName $ symbolVal (Proxy @cls)) ExtendsObject $ emptyDefClsInn

emptyDefClsInn ::  DefClsInn
emptyDefClsInn  = DefClsInn mempty [] [] [] []


-- | Join two DefClsInns by making a union of enums and union of its fields
joinDefClsInn :: DefClsInn -> DefClsInn -> DefClsInn
joinDefClsInn dci0 dci1 = DefClsInn (unionWith (<>) (_dciDefEnums dci0) (_dciDefEnums dci1)) [] [] (_dciDefVars dci0 <> _dciDefVars dci1) (_dciDefFuncs dci0 <> _dciDefFuncs dci1)

instance Semigroup DefClsInn where (<>) = joinDefClsInn
instance Monoid DefClsInn where mempty =  emptyDefClsInn

addToEnum :: String -> String -> DefClsInn -> DefClsInn
addToEnum k v = dciDefEnums %~ insertWith (flip (<>)) k [EnumVal v]

-- Rendering of .gd files

formatDefCls :: DefCls -> String
formatDefCls (DefCls (ClsName cls) ext (DefClsInn ens _ csts vars funcs))  = [i|
class_name #{cls}

extends #{ext}

#{unlines $ fmap formatEnum $ toList ens   }
#{unlines $ fmap formatVar vars   }
#{unlines $ fmap formatFunc funcs   }

|]

formatEnum :: (String, [EnumVal]) -> String
formatEnum (enm, vals) = [i|enum #{enm} { #{intercalate ", " $ fmap evVal vals} } |]

formatVar :: DefVar -> String
formatVar (DefVar nm typ ) = [i|var #{formatVarName nm}: #{formatTyp typ} |]

formatVarName (VarName nm) = nm

formatFunc :: DefFunc -> String
formatFunc (DefFunc isSt nm [] [] []) = [i|#{if isSt then "static " else ""}func #{nm}() |]

formatTyp Nothing = ""
formatTyp (Just (TypPrim PTInt   )) = "int"
formatTyp (Just (TypPrim PTFloat )) = "flaot"
formatTyp (Just (TypPrim PTString)) = "String"
formatTyp (Just (TypPrim PTBool  )) = "bool"
formatTyp (Just (TypPrim PTV2    )) = "Vector2"
formatTyp (Just (TypPrim PTV3    )) = "Vector3"
formatTyp (Just (TypCls (ClsName nm))) = nm
formatTyp (Just t) = show t

-- | Enrich DefCls with serialization and deserialization functions compatible with godot-ser serialization
addSerialization :: DefCls -> DefCls
addSerialization = dcInn . dciDefFuncs %~
  (<> [DefFunc True (FuncName "show") [] [] []])


class ToTyp t where
  toTyp :: Typ

instance ToTyp Int where toTyp = TypPrim PTInt
instance ToTyp Double where toTyp = TypPrim PTFloat
instance ToTyp Float where toTyp = TypPrim PTFloat
instance ToTyp String where toTyp = TypPrim PTString
instance ToTyp (V2 n) where toTyp = TypPrim PTV2

