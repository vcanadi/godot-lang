{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

{- | Core module that defines structure of GD script language
-}
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
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import Control.Monad (join)

class ToTyp t where
  toTyp :: Typ

instance ToTyp Int where toTyp = TypPrim PTInt
instance ToTyp Double where toTyp = TypPrim PTFloat
instance ToTyp Float where toTyp = TypPrim PTFloat
instance ToTyp (V2 n) where toTyp = TypPrim PTV2
instance (ToTyp a, ToTyp b) => ToTyp (a, b) where toTyp = TypPair (toTyp @a) (toTyp @b)
instance {-# OVERLAPPABLE #-} ToTyp a => ToTyp [a] where toTyp = TypArr (toTyp @a)
instance ToTyp String where toTyp = TypPrim PTString
instance (ToTyp k, ToTyp v) => ToTyp (Map k v) where toTyp = TypDict (toTyp @k) (toTyp @v)


-- Godot language AST

-- | Name of the module
newtype ClsName = ClsName { cnName :: String } deriving (Eq, Show)

-- | Enum values
newtype EnumVal = EnumVal { evVal :: String } deriving (Eq, Ord, Show)

-- | Variable
newtype VarName = VarName { vnName :: String } deriving (Eq, Show )

-- | Identifier
newtype Iden = Iden { idenId :: [String] }  deriving(Eq, Show, Semigroup, Monoid)

-- | Call to some godot function
newtype FuncName = FuncName { cfFunc :: String } deriving (Eq, Show)

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
  PTByteArr :: PrimTyp
deriving instance Eq PrimTyp
deriving instance Show PrimTyp

-- | Godot primitive value (e.g. 5, 4.0, "abc")
data PrimVal  where
  PVInt    :: Int       -> PrimVal
  PVFloat  :: Double    -> PrimVal
  PVString :: String    -> PrimVal
  PVBool   :: Bool      -> PrimVal
  PVV2     :: V2 Double -> PrimVal
  PVV3     :: V3 Double -> PrimVal
deriving instance Eq PrimVal
deriving instance Show PrimVal

-- | Godot class value (list of optionally set fields)
newtype ClsVal = ClsVal { cvVars :: Map VarName Val } deriving (Eq, Show)

-- | Godot array type label
data ArrTyp = ArrTyp deriving (Eq,Show)

-- | Godot array value
newtype ArrVal a = ArrVal [a] deriving (Eq,Show)

-- | Any godot type (primitives + custom classes + arrays)
data Typ where
  TypPrim :: PrimTyp -> Typ -- ^ Primitive type
  TypCls :: ClsName -> Typ -- ^ Custom class
  TypArr :: Typ -> Typ -- ^ Array
  TypPair :: Typ -> Typ -> Typ -- ^ Pair of values
  TypDict :: Typ -> Typ -> Typ -- ^ Dictionary
  TypEnum :: String -> Typ -- ^ Enum type
  TypAny :: Typ  -- ^ Any type flag
deriving instance Eq Typ
deriving instance Show Typ

data Val where
  ValPrim :: PrimVal -> Val
  ValCls :: ClsVal -> Val
  ValEnum :: EnumVal -> Val
deriving instance Eq Val
deriving instance Show Val

-- | Main type representing GD script AST
newtype Script = Script
  { gdsClass :: DefCls
  }

-- | Variable declaration
data DefVar = DefVar
  { varName :: VarName
  , varType :: Typ
  } deriving (Eq,Show)

-- | Easier DefVar construction
(-::) :: String -> Typ -> DefVar
(-::) = DefVar . VarName

-- | Easier DefVar construction
defVar' :: forall typ. (ToTyp typ) => String -> DefVar
defVar' vn = DefVar (VarName vn) (toTyp @typ)

data DefFunc = DefFunc
  { _dfIsStat :: Bool
  , _dfComment :: Maybe String
  , _dfName :: FuncName
  , _dfArgs :: [DefVar]
  , _dfOutTyp :: Typ
  , _dfLocalVars :: [DefVar]
  , _dfStmts :: [Stmt]
  }
deriving instance Show DefFunc

-- | Combinator for easier DefFunc construction
defFunc :: String -> String -> [DefVar] -> Typ -> [DefVar] -> [Stmt] -> DefFunc
defFunc comm fn = DefFunc False (Just comm) (FuncName fn)

-- | Combinator for easier static DefFunc construction
defStatFunc :: String -> String -> [DefVar] -> Typ -> [DefVar] -> [Stmt] -> DefFunc
defStatFunc comm fn = DefFunc True (Just comm) (FuncName fn)

data Stmt where
  StmtApp :: Expr App -> Stmt
  StmtIf :: Expr Bool -> Stmt -> Stmt
  StmtIfElse :: Expr Bool -> Stmt -> Stmt -> Stmt
  StmtFor :: VarName -> (Expr Enumerable) -> [Stmt] -> Stmt
  StmtMatch :: Expr r -> [(Expr r, [Stmt])] -> Stmt
  StmtRet :: Expr r -> Stmt
  StmtVarInit :: DefVar -> Maybe (Expr t) -> Stmt
  StmtSet :: Iden -> Expr t -> Stmt
deriving instance Show Stmt

-- Combinators for building statements

-- | Var initialization statement helper
(-:=) :: DefVar -> Expr t -> Stmt
(-:=) dv val = StmtVarInit dv $ Just val

-- | Set statement helper
(--=) :: [String] -> Expr t -> Stmt
(--=) ids  = StmtSet (Iden ids)

-- | Type used for flaging godot expression acceptable for for loop
data Enumerable = Enumerable

-- | Type used for flaging godot raw expression
data Raw = Raw

-- | Type used for flaging godot String expression
data Str = Str

-- | Type used for flaging godot Array expression
data Arr = Arr

-- | Type used for flaging godot function abstraction expression
data Lam = Lam

-- | Type used for flaging godot function application expression
data App = App

data ExprElem = forall t. ExprElem { eeElem :: Expr t }
data ExprAny = ExprAnyCons
deriving instance Show ExprElem

data Expr t where
  ExprFalse :: Expr Bool
  ExprTrue :: Expr Bool
  ExprRange :: Int -> Int -> Int -> Expr Enumerable
  ExprRangeVar :: VarName -> Expr Enumerable
  ExprStr :: String -> Expr Str
  ExprArr :: [ExprElem] -> Expr Arr
  ExprLam :: VarName -> Expr t -> Expr Lam
  ExprApp :: FuncName -> [Expr t] -> Expr App
  ExprRaw :: String -> Expr Raw
  ExprAny :: Expr t -> Expr ExprAny -- ^ Wrap any expression in opaque type
deriving instance Show (Expr t)

(-->) vn = ExprLam (VarName vn)
(--$) fn = ExprApp (FuncName fn)

-- | Main type describing godot class, inner information about class
data DefClsInn = DefClsInn
  { _dciDefEnums :: Map String [EnumVal] -- ^ Local class enums
  , _dciDefClasses :: [DefCls]
  , _dciDefConsts :: [DefVar]
  , _dciDefConVars :: Map EnumVal [DefVar] -- ^ Unique enum "Con" with optional variables belonging to it
  , _dciDefVars :: [DefVar]
  , _dciDefFuncs :: [DefFunc]
  } deriving Show

-- | Defintion of a godot type/class
data DefCls = DefCls
  { _dcName :: ClsName
  , _dcExtends :: Extends
  , _dcInn :: DefClsInn
  } deriving Show

$(makeLenses ''DefClsInn)
$(makeLenses ''DefCls)

-- Combinators for building DefCls

-- | Godot's class definition without any functions, enums, variables,...
emptyDefClsInn ::  DefClsInn
emptyDefClsInn  = DefClsInn mempty [] [] mempty [] []

-- | Join two DefClsInns by making a union of enums and union of its fields
joinDefClsInn :: DefClsInn -> DefClsInn -> DefClsInn
joinDefClsInn dci0 dci1 =
  DefClsInn
    (unionWith (<>) (_dciDefEnums dci0) (_dciDefEnums dci1))
    (_dciDefClasses dci0 <> _dciDefClasses dci1)
    (_dciDefConsts dci0 <> _dciDefConsts dci1)
    (unionWith (<>) (_dciDefConVars dci0) (_dciDefConVars dci1))
    (_dciDefVars dci0 <> _dciDefVars dci1)
    (_dciDefFuncs dci0 <> _dciDefFuncs dci1)

instance Semigroup DefClsInn where (<>) = joinDefClsInn
instance Monoid DefClsInn where mempty =  emptyDefClsInn

-- | Add new value to some enum
addToEnum :: String -> String -> DefClsInn -> DefClsInn
addToEnum k v = dciDefEnums %~ insertWith (flip (<>)) k [EnumVal v]

-- | Add a value to unique "Con" enum representing sum type's constructor flag
addToConEnum :: String -> DefClsInn -> DefClsInn
addToConEnum v = dciDefConVars %~ M.insert (EnumVal v) []

-- | Add a variable definition that holds some sum type constructor's record value
addRecDefVar :: forall fld typ. (KnownSymbol fld, ToTyp typ) => EnumVal -> DefClsInn -> DefClsInn
addRecDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [defVar' @typ ("fld_" <> evVal con <> "_" <> symbolVal (Proxy @fld)) ]

-- | Add a variable definition that holds some sum type constructor's unnamed value
addConDefVar :: forall typ. ToTyp typ => Int -> EnumVal -> DefClsInn -> DefClsInn
addConDefVar i con = dciDefConVars %~ insertWith (flip (<>)) con [defVar' @typ ("fld_" <> evVal con <> "_" <> show i)]

-- | Add function definition
addDefFunc :: DefFunc -> DefCls -> DefCls
addDefFunc f dc = dc & over (dcInn . dciDefFuncs) (<> [f])

-- | Add multple function definitions
addDefFuncs :: [DefFunc] -> DefCls -> DefCls
addDefFuncs fs dc = dc & over (dcInn . dciDefFuncs) (<> fs)
