{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Lens
import Data.Map.Strict (Map, insertWith)
import qualified Data.Map.Strict as M
import Control.Arrow ((>>>))
import Data.List (find)
import Data.Maybe (isNothing)

-- | Class that defines how to represent/translate a Haskell type name to Godot type name
class ToTyp t where
  toTyp :: Typ

instance ToTyp Int where toTyp = TypPrim PTInt
instance ToTyp Double where toTyp = TypPrim PTFloat
instance ToTyp Float where toTyp = TypPrim PTFloat
instance ToTyp (V2 n) where toTyp = TypPrim PTV2
instance (ToTyp a, ToTyp b) => ToTyp (a, b) where toTyp = TypPair (toTyp @a) (toTyp @b)
instance {-# OVERLAPPABLE #-} ToTyp a => ToTyp [a] where toTyp = TypArr (toTyp @a)
instance ToTyp String where toTyp = TypPrim PTString
instance (ToTyp k, ToTyp v) => ToTyp (Map k v) where toTyp = TypArr (TypPair (toTyp @k) (toTyp @v)) -- ^ Represent Map as a list of pairs instead of Dictionary to keep the type information


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
  TypPair :: Typ -> Typ -> Typ -- ^ Pair of values. Special type that is represented as custom inner type (since godot doesn't have type parameters)
  TypDict :: Typ -> Typ -> Typ -- ^ Dictionary
  TypEnum :: String -> Typ -- ^ Enum type
  TypAny :: Typ  -- ^ Any type flag
deriving instance Eq Typ
deriving instance Show Typ

-- showTyp :: Typ -> String
-- showTyp (TypPrim pt) = show pt
-- showTyp (TypCls (ClsName nm)) = nm
-- showTyp (TypArr _) = "Arr"
-- showTyp (TypPair a b) = "Pair<" <> showTyp a <> ", " <>  showTyp b <> ">"
-- showTyp (TypDict _ _) = "Dict"
-- showTyp (TypEnum _) = "Enum"
-- showTyp TypAny = "Any"

showTyp :: Typ -> String
showTyp = f 0
  where
    f :: Int -> Typ -> String
    f i (TypArr t)            = "Array[" <>  f (succ i) t <> "]"
    f i (TypDict t t')        = "Dictionary[" <> f (succ i) t <> ", " <> f (succ i) t' <> "]"
    f _ t = pairName t

-- | Build Type name for Pair types
pairName :: Typ -> String
pairName = f 0
  where
    f :: Int -> Typ -> String
    f _ (TypPrim PTInt    )   = "int"
    f _ (TypPrim PTFloat  )   = "float"
    f _ (TypPrim PTString )   = "String"
    f _ (TypPrim PTBool   )   = "bool"
    f _ (TypPrim PTV2     )   = "Vector2"
    f _ (TypPrim PTV3     )   = "Vector3"
    f _ (TypPrim PTByteArr)   = "PackedByteArray"
    f i (TypArr t)            = "A_" <> f (succ i) t <> "_A"
    f i (TypPair t t')        = "P_" <> f (succ i) t <> "_" <> f (succ i) t' <> "_P"
    f i (TypDict t t')        = "D_" <> f (succ i) t <> "_" <> f (succ i) t' <> "_D"
    f _ (TypCls (ClsName nm)) = nm
    f _ (TypEnum enm) = enm
    f _ TypAny  = "Variant"

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
func :: String -> [DefVar] -> Typ -> [Stmt] -> DefFunc
func fn args typ = DefFunc False Nothing (FuncName fn) args typ []

-- | Combinator for easier static DefFunc construction
stat_func :: String -> [DefVar] -> Typ -> [Stmt] -> DefFunc
stat_func fn args typ = DefFunc True Nothing (FuncName fn) args typ []

(###) :: String -> DefFunc -> DefFunc
comment ### f = f { _dfComment = Just comment }

data Stmt where
  StmtApp :: Expr App -> Stmt
  StmtIf :: Expr Bool -> Stmt -> Stmt
  StmtIfElse :: Expr Bool -> Stmt -> Stmt -> Stmt
  StmtFor :: VarName -> (Expr Enumerable) -> [Stmt] -> Stmt
  StmtMatch :: Expr r -> ([(Expr r, [Stmt])], Maybe [Stmt])  -> Stmt
  StmtRet :: Expr r -> Stmt
  StmtVarInit :: DefVar -> Maybe (Expr t) -> Stmt
  StmtSet :: Iden -> Expr t -> Stmt
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

data ExprElem = forall t. EElem { eeElem :: Expr t }
deriving instance Show ExprElem

data Expr t where
  EFalse :: Expr Bool
  ETrue :: Expr Bool
  EEq :: Expr t -> Expr t' -> Expr Bool
  EOr :: [Expr Bool] -> Expr Bool
  EAnd :: [Expr Bool] -> Expr Bool
  ENot :: Expr Bool -> Expr Bool
  ERange :: Int -> Int -> Int -> Expr Enumerable
  ERangeVar :: VarName -> Expr Enumerable
  EStr :: String -> Expr Str
  EArr :: [ExprElem] -> Expr Arr
  EAt  :: Int -> Expr t -> Expr t'
  ELam :: [VarName] -> Expr t -> Expr Lam
  EApp :: FuncName -> [Expr t] -> Expr t'
  ERaw :: String -> Expr t
  EId :: Iden -> Expr t -- ^ Identifier
deriving instance Show (Expr t)

(-->) :: String -> Expr t -> Expr Lam
(-->) vn = ELam [VarName vn]

(--$) :: String -> [Expr t] -> Expr t'
(--$) fn = EApp (FuncName fn)

-- | Main type describing godot class, inner information about class
data DefClsInn = DefClsInn
  { _dciDefEnums :: Map String [EnumVal] -- ^ Local class enums
  , _dciDefClasses :: [DefCls]
  , _dciDefConsts :: [DefVar]
  , _dciDefConVars :: [(EnumVal, [DefVar])] -- ^ Unique enum "Con" with optional variables belonging to it
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

-- | Helper function. Like unionWith on Map k v, but for [(k,v)]
unionWithL :: (Eq k) => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
unionWithL f xs ys = [ find (fst >>> (==xk)) ys  & \case Nothing -> (xk, xv); Just (_, yv) -> (xk, f xv yv)
                    | (xk, xv) <- xs]
                 <> [ (yk, yv)
                    | (yk, yv) <- ys, isNothing $ find (fst >>> (==yk)) xs]

insertWithL :: (Eq k) => (v -> v -> v) -> k -> v -> [(k,v)] -> [(k,v)]
insertWithL f k v xs = unionWithL f xs [(k,v)]

-- Combinators for building DefCls

-- | Godot's class definition without any functions, enums, variables,...
emptyDefClsInn ::  DefClsInn
emptyDefClsInn  = DefClsInn mempty [] [] mempty [] []

-- | Join two DefClsInns by making a union of enums and union of its fields
joinDefClsInn :: DefClsInn -> DefClsInn -> DefClsInn
joinDefClsInn dci0 dci1 =
  DefClsInn
    (M.unionWith (<>) (_dciDefEnums dci0) (_dciDefEnums dci1))
    (_dciDefClasses dci0 <> _dciDefClasses dci1)
    (_dciDefConsts dci0 <> _dciDefConsts dci1)
    (unionWithL (<>) (_dciDefConVars dci0) (_dciDefConVars dci1))
    (_dciDefVars dci0 <> _dciDefVars dci1)
    (_dciDefFuncs dci0 <> _dciDefFuncs dci1)

instance Semigroup DefClsInn where (<>) = joinDefClsInn
instance Monoid DefClsInn where mempty =  emptyDefClsInn

-- | Add new value to some enum
addToEnum :: String -> String -> DefClsInn -> DefClsInn
addToEnum k v = dciDefEnums %~ insertWith (flip (<>)) k [EnumVal v]

-- | Add a value to unique "Con" enum representing sum type's constructor flag
addToConEnum :: String -> DefClsInn -> DefClsInn
addToConEnum v = dciDefConVars %~ (<> [(EnumVal v, [])])

-- | Add a variable definition that holds some sum type constructor's record value
-- Additionally construct class inner type in case that is wanted (e.g. if type is a TypPair (helper type collection mimicing type variables))
addRecDefVar :: String -> Typ -> EnumVal -> DefClsInn -> DefClsInn
addRecDefVar fld typ con = (dciDefConVars %~ insertWithL ((<>)) con [fld -:: typ ])
                         . (case typ of (TypArr (TypPair a b)) -> addTypPairCls a b; _ -> id)

-- | Add a variable definition that holds some sum type constructor's unnamed value
addConDefVar :: Int -> Typ -> EnumVal -> DefClsInn -> DefClsInn
addConDefVar i typ con = (dciDefConVars %~ insertWithL (flip (<>)) con [("fld_" <> evVal con <> "_" <> show i) -:: typ])
                       . (case typ of (TypArr (TypPair a b)) -> addTypPairCls a b
                                      (TypPair a b) -> addTypPairCls a b
                                      _ -> id)

addTypPairCls :: Typ -> Typ -> DefClsInn -> DefClsInn
addTypPairCls a b = dciDefClasses %~ (mkTypPairCls:)
  where
    mkTypPairCls = DefCls (ClsName $ pairName (TypPair a b)) ExtendsObject
                     (emptyDefClsInn & addRecDefVar "fst" a (EnumVal "P")
                                     & addRecDefVar "snd" b (EnumVal "P"))

-- | Add functions to the class and every class defined within
addFuncsRecursive :: (DefCls -> [DefFunc]) -> DefCls -> DefCls
addFuncsRecursive mkFs dc = (dcInn . dciDefFuncs %~ (<> mkFs dc))
                        $ (dcInn . dciDefClasses %~ fmap (addFuncsRecursive mkFs))
                        dc

-- | Check if type has multiple constructors
isSumType :: DefCls -> Bool
isSumType = (>1) . length . fmap fst . _dciDefConVars . _dcInn

-- | Check if it has single constructor and single variable (newtype or data)
isNewtype :: DefCls -> Bool
isNewtype = _dcInn >>> _dciDefConVars >>> (\case [(_,[_])] -> True; _ -> False)

-- | Check if each constructor has 0 arguments
isEnum :: DefCls -> Bool
isEnum = _dcInn >>> _dciDefConVars >>> all (snd >>> null)


-- Combinators for building statements and expressions

-- Assignments
--
-- | Var initialization statement helper
(-:=) :: DefVar -> Expr t -> Stmt
(-:=) dv val = StmtVarInit dv $ Just val

-- | Set statement helper
(--=) :: [String] -> Expr t -> Stmt
(--=) ids  = StmtSet (Iden ids)

-- | Easier DefVar construction
(-::) :: String -> Typ -> DefVar
(-::) = DefVar . VarName

-- Binary operators
--
-- | Equality check
(-==) :: Expr t -> Expr t' -> Expr Bool
a -== b = EEq a b

(-&&) :: Expr Bool -> Expr Bool -> Expr Bool
a -&& (EAnd bs) = EAnd $ a:bs
(EAnd as) -&& b = EAnd $ as <> [b]
a -&& b = EAnd [a, b]

(-||) :: Expr Bool -> Expr Bool -> Expr Bool
a -|| (EOr bs) = EOr $ a:bs
(EOr as) -|| b = EOr $ as <> [b]
a -|| b = EOr [a, b]


-- | Shortcut for some constructor constant "Con.<con>: Con"
eCon :: String -> Expr t
eCon con = EId $ Iden ["Con",con]

eId :: [String] -> Expr t
eId is = EId $ Iden is
