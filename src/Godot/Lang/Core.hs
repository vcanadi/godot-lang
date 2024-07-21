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
{-# LANGUAGE PolyKinds #-}


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
instance ToTyp String where toTyp = TypPrim PTString
instance ToTyp (V2 n) where toTyp = TypPrim PTV2


-- Godot language AST

-- | Name of the module
newtype ClsName = ClsName { cnName :: String } deriving (Eq, Show, Semigroup, Monoid)

-- | Enum values
newtype EnumVal = EnumVal { evVal :: String } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Identifier
newtype VarName = VarName { vnName :: String } deriving (Eq, Show, Semigroup, Monoid, Ord)

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

-- | Godot array vlue
newtype ArrVal a = ArrVal [a] deriving (Eq,Show)

-- | Any godot type (primitives + custom classes + arrays)
data Typ where
  TypPrim :: PrimTyp -> Typ
  TypCls :: ClsName -> Typ
  TypEnum :: String -> Typ
deriving instance Eq Typ
deriving instance Show Typ

isPrimTyp :: Typ -> Bool
isPrimTyp (TypPrim _) = True
isPrimTyp _ = False

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

-- | Combinator for easier DefVar construction
defVar :: forall typ. (ToTyp typ) => String -> DefVar
defVar vn = DefVar (VarName vn) (toTyp @typ)

data DefFunc = DefFunc
  { _dfIsStat :: Bool
  , _dfName :: FuncName
  , _dfArgs :: [DefVar]
  , _dfOutTyp :: Typ
  , _dfLocalVars :: [DefVar]
  , _dfStmts :: forall t. [Stmt t]
  }
deriving instance Show DefFunc

-- | Combinator for easier DefFunc construction
defFunc :: String -> [DefVar] -> Typ -> [DefVar] -> (forall (t :: k). [Stmt t]) -> DefFunc
defFunc fn = DefFunc False (FuncName fn)

-- | Combinator for easier static DefFunc construction
defStatFunc :: String -> [DefVar] -> Typ -> [DefVar] -> (forall (t :: k). [Stmt t]) -> DefFunc
defStatFunc fn = DefFunc True (FuncName fn)

data Stmt t where
  StmtCallFunc :: FuncName -> Stmt t
  StmtIf :: Expr Bool -> Stmt t -> Stmt t
  StmtIfElse :: Expr Bool -> Stmt t -> Stmt t -> Stmt t
  StmtFor :: VarName -> (Expr Enumerable) -> Stmt t -> Stmt t
  StmtMatch :: Expr r -> [(Expr r, Stmt t)] -> Stmt t
  StmtRet :: Expr r -> Stmt t
  StmtVarInit :: DefVar -> Maybe (Expr t) -> Stmt r
  StmtSet :: [VarName] -> Expr t -> Stmt r
deriving instance Show (Stmt t)

-- | Type used for flaging godot expression acceptable for for loop
data Enumerable = Enumerable

-- | Type used for flaging godot raw expression
data Raw = Raw

-- | Type used for flaging godot String expression
data Str = Str

-- | Type used for flaging godot Array expression
data Arr = Arr

data Expr t where
  ExprFalse :: Expr Bool
  ExprTrue :: Expr Bool
  ExprRange :: Int -> Int -> Int -> Expr Enumerable
  ExprRangeVar :: VarName -> Expr Enumerable
  ExprStr :: String -> Expr Str
  ExprArr :: [Expr t'] -> Expr Arr
  ExprRaw :: String -> Expr Raw

-- deriving instance Eq (Expr t)
deriving instance Show (Expr t)

-- | Main type describing godot class
data DefClsInn = DefClsInn
  { _dciDefEnums :: Map String [EnumVal] -- ^ Local class enums
  , _dciDefClasses :: [DefCls]
  , _dciDefConsts :: [DefVar]
  , _dciDefConVars :: Map EnumVal [DefVar] -- ^ Constructor enum with optional variables belonging to it
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

addToEnum :: String -> String -> DefClsInn -> DefClsInn
addToEnum k v = dciDefEnums %~ insertWith (flip (<>)) k [EnumVal v]

addToConEnum :: String -> DefClsInn -> DefClsInn
addToConEnum v = dciDefConVars %~ M.insert (EnumVal v) []

addConDefVar :: forall field typ. (KnownSymbol field, ToTyp typ) => EnumVal -> DefClsInn -> DefClsInn
addConDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [defVar @typ ("field_" <> evVal con <> "_" <> symbolVal (Proxy @field)) ]

addUnConDefVar :: forall typ. ToTyp typ => EnumVal -> DefClsInn -> DefClsInn
addUnConDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [defVar @typ ("field_" <> evVal con)]

addDefFunc :: DefFunc -> DefCls -> DefCls
addDefFunc f dc = dc & over (dcInn . dciDefFuncs) (<> [f])

addDefFuncs :: [DefFunc] -> DefCls -> DefCls
addDefFuncs fs dc = dc & over (dcInn . dciDefFuncs) (<> fs)

addBasicFunctions :: DefCls -> DefCls
addBasicFunctions = addSerialization
                  . addConShow
                  . addCons

-- | Enrich DefCls with "con" enum with show function
addConShow :: DefCls -> DefCls
addConShow dc = (`addDefFunc` dc) $
  DefFunc False (FuncName "show") [] (TypPrim PTString) []
        [ StmtMatch (ExprRaw "self.con")
           [(ExprRaw $ "Con." <> con , StmtRet (ExprStr con)) | (EnumVal con,_) <- toList $ _dciDefConVars $ _dcInn dc]
        ]

-- | Enrich DefCls with "constructor" functions for each constructor in sum type
addCons :: DefCls -> DefCls
addCons dc = (`addDefFuncs` dc) $ ((toList $ _dciDefConVars $ _dcInn dc) <&>) $ \(EnumVal con, vs) ->
  defStatFunc (toLower <$> con) (join $ maybeToList $ M.lookup (EnumVal con) $ _dciDefConVars $ _dcInn dc) (TypCls $ _dcName dc) [] $
    [ StmtVarInit (DefVar (VarName "this") (TypCls $ _dcName dc )) (Just $ ExprRaw $ cnName (_dcName dc) <> ".new()") ] <>
    [ StmtSet [VarName "this", VarName "con"] (ExprRaw $ "Con." <> con) ] <>
    [ StmtSet [VarName "this", vn] (ExprRaw (vnName vn)) | DefVar vn _ <- vs ] <>
    [ StmtRet (ExprRaw "this") ]

-- | Enrich DefCls with serialization function compatible with godot-ser serialization
addSerialization :: DefCls -> DefCls
addSerialization dc = (`addDefFuncs` dc)
  -- | Serialize to nested array
  [ defStatFunc "serArr" [DefVar (VarName "this") $ TypCls $ _dcName dc] (TypPrim PTArr) []
      [ StmtMatch (ExprRaw "this.con") $ ((toList $ _dciDefConVars $ _dcInn dc) <&>) $ \(EnumVal con, vs) ->
         ( ExprRaw $ "Con." <> con
         , StmtRet $ ExprArr $
             ((ExprRaw $ "Con." <> con) :) $ (vs <&>) $ \(DefVar (VarName vn) typ) ->
                 if isPrimTyp typ
                    then ExprRaw vn
                    else ExprRaw $ vn <> ".serArr()"
         )
      ]
  -- | Serialize to binary
  , defStatFunc "ser" [DefVar (VarName "this") $ TypCls $ _dcName dc] (TypPrim PTByteArr) []
      [StmtRet $ ExprRaw "var_to_bytes(serArr(this))"]
  ]

