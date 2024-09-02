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
instance {-# OVERLAPPABLE #-} (ToTyp a) => ToTyp [a] where toTyp = TypArr (toTyp @a)
instance ToTyp String where toTyp = TypPrim PTString
instance ToTyp (Map k a) where toTyp = TypDict


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
  TypDict :: Typ -- ^ Dictionary
  TypEnum :: String -> Typ -- ^ Enum type
  TypAny :: Typ  -- ^ Any type flag
deriving instance Eq Typ
deriving instance Show Typ

isPrimTyp :: Typ -> Bool
isPrimTyp (TypPrim _) = True
isPrimTyp _ = False

isArrTyp :: Typ -> Bool
isArrTyp (TypArr _) = True
isArrTyp _ = False


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
  StmtFor :: VarName -> (Expr Enumerable) -> Stmt -> Stmt
  StmtMatch :: Expr r -> [(Expr r, [Stmt])] -> Stmt
  StmtRet :: Expr r -> Stmt
  StmtVarInit :: DefVar -> Maybe (Expr t) -> Stmt
  StmtSet :: [VarName] -> Expr t -> Stmt
deriving instance Show Stmt

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

addRecConDefVar :: forall field typ. (KnownSymbol field, ToTyp typ) => EnumVal -> DefClsInn -> DefClsInn
addRecConDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [defVar @typ ("field_" <> evVal con <> "_" <> symbolVal (Proxy @field)) ]

addConDefVar :: forall typ. ToTyp typ => EnumVal -> DefClsInn -> DefClsInn
addConDefVar con = dciDefConVars %~ insertWith (flip (<>)) con [defVar @typ ("field_" <> evVal con)]

addDefFunc :: DefFunc -> DefCls -> DefCls
addDefFunc f dc = dc & over (dcInn . dciDefFuncs) (<> [f])

addDefFuncs :: [DefFunc] -> DefCls -> DefCls
addDefFuncs fs dc = dc & over (dcInn . dciDefFuncs) (<> fs)

addBasicFunctions :: DefCls -> DefCls
addBasicFunctions = addSerialization
                  . addDeserialization
                  . addConShow
                  . addCons

-- | Enrich DefCls with "con" enum with show function
addConShow :: DefCls -> DefCls
addConShow dc = (`addDefFunc` dc) $
  DefFunc False (Just "String representation of type") (FuncName "show") [] (TypPrim PTString) []
        [ StmtMatch (ExprRaw "self.con")
           [(conExpr con , [StmtRet (ExprStr con)]) | (EnumVal con,_) <- toList $ _dciDefConVars $ _dcInn dc]
        ]

-- | Enrich DefCls with "constructor" functions for each constructor in sum type
addCons :: DefCls -> DefCls
addCons dc = (`addDefFuncs` dc) $ ((toList $ _dciDefConVars $ _dcInn dc) <&>) $ \(EnumVal con, vs) ->
  defStatFunc ("Constructor function for sum constructor " <> con)
    (toLower <$> con) (join $ maybeToList $ M.lookup (EnumVal con) $ _dciDefConVars $ _dcInn dc) (TypCls $ _dcName dc) [] $
    [ StmtVarInit (DefVar (VarName "ret") (TypCls $ _dcName dc )) (Just $ ExprRaw $ cnName (_dcName dc) <> ".new()") ] <>
    [ StmtSet [VarName "ret", VarName "con"] (conExpr con) ] <>
    [ StmtSet [VarName "ret", vn] (ExprRaw (vnName vn)) | DefVar vn _ <- vs ] <>
    [ StmtRet (ExprRaw "ret") ]

-- Serilization expressions
--
-- | Enrich DefCls with serialization function compatible with godot-ser serialization
addSerialization :: DefCls -> DefCls
addSerialization dc = (`addDefFuncs` dc)
  [ defStatFunc "Serialize to array"
      "serToArr" [DefVar (VarName "this") $ TypCls $ _dcName dc] (TypArr TypAny) []
      [ StmtMatch (ExprRaw "this.con") $ ((toList $ _dciDefConVars $ _dcInn dc) <&>) $ \(EnumVal con, vs) ->
         ( conExpr con , [ StmtRet $ clsSerExpr con vs])
      ]
  , defStatFunc "ser" "Serialize to binary" [DefVar (VarName "this") $ TypCls $ _dcName dc] (TypPrim PTByteArr) []
      [StmtRet $ ExprRaw "var_to_bytes(serToArr(this))"]
  ]

conExpr con = ExprRaw $ "Con." <> con

-- | gd expression that serializes class based on selected constructor
clsSerExpr :: String -> [DefVar] -> Expr Arr
clsSerExpr con vs =
  ExprArr $
    [ ExprElem $ conExpr con ] <>
    [ case typ of
        TypCls _    -> ExprElem $ ExprRaw $ vn <> ".serToArr()"
        TypArr typ' -> ExprElem $ arrSerExpr vn typ'
        _           -> ExprElem (ExprRaw vn)
    | (DefVar (VarName vn) typ) <- vs
    ]

-- | gd expression that serializes arrays
arrSerExpr nm (TypArr t')           = ExprAny $ ExprApp (FuncName $ nm <> ".map") [ExprLam (VarName "x") $ arrSerExpr "x" t']
arrSerExpr nm (TypCls (ClsName cn)) = ExprAny $ ExprApp (FuncName $ nm <> ".map") [ExprRaw  "serToArr"]
arrSerExpr nm _                     = ExprAny $ ExprRaw nm

-- | gd statements that deserializes class based on selected constructor
clsDesStmts con vs = concat
  [ case typ of
      TypCls _    -> [ StmtSet [VarName "ret", VarName vn] $ ExprRaw $ "arr[" <> show i <> "].desFromArr()" ]
      TypArr typ' -> -- [ StmtSet [VarName "ret", VarName vn] $ ExprRaw $ show typ <> ".new()"
                     [ StmtApp $ ExprApp  (FuncName $ "ret." <>  vn <> ".assign") [arrDesExpr "arr" typ']
                     ]
      _           -> [StmtSet [VarName "ret", VarName vn] $ ExprRaw $ "arr[" <> show i <> "]"]

   -- StmtSet [VarName "ret", VarName vn] (ExprRaw $ "arr[" <> show i <> "]" <> (if isPrimTyp typ then "" else ".desFromArr()"))
  | (i, DefVar (VarName vn) typ) <- zip [1..] vs
  ]


-- | gd expression that serializes arrays
arrDesExpr nm (TypArr t')          = ExprAny $ ExprApp (FuncName $ nm <> ".map") [ExprLam (VarName "x") $ arrDesExpr "x" t']
arrDesExpr nm (TypCls (ClsName cn)) = ExprAny $ ExprApp (FuncName $ nm <> ".map") [ExprRaw "desFromArr"]
arrDesExpr nm _                     = ExprAny $ ExprRaw nm

-- Deserilization expressions

-- | Enrich DefCls with deserialization function compatible with godot-ser serialization
addDeserialization :: DefCls -> DefCls
addDeserialization dc = (`addDefFuncs` dc)
  [ defStatFunc "Deserialize from array"
      "desFromArr"
      [DefVar (VarName "arr") (TypArr TypAny)] (TypCls $ _dcName dc) []
      [ StmtVarInit (DefVar (VarName "ret") (TypCls $ _dcName dc )) (Just $ ExprRaw $ cnName (_dcName dc) <> ".new()")
      , StmtMatch (ExprRaw "arr[0]") $ ((filter (not . null . snd) $ toList $ _dciDefConVars $ _dcInn dc) <&>) $ \(EnumVal con, vs) ->
         ( conExpr con, clsDesStmts con vs)
      , StmtRet (ExprRaw "ret")
      ]
      -- [ StmtVarInit (DefVar (VarName "this") (TypCls $ _dcName dc )) (Just $ ExprRaw $ cnName (_dcName dc) <> ".new()")
      -- , StmtSet [VarName "this", VarName "con"] (ExprRaw "arr[0]")
      -- ] <>
      -- [ StmtSet [VarName "this", v] (ExprRaw "arr[0]")
      -- | DefVar v _ <- join $ maybeToList $ M.lookup (EnumVal con) $ _dciDefConVars $ _dcInn dc]


  , defStatFunc "Deserialize from binary"
      "des" [DefVar (VarName "this") $ TypPrim PTByteArr] (TypCls $ _dcName dc) []
      [StmtRet $ ExprRaw "desFromArr(bytes_to_var(bs))"]
  ]



