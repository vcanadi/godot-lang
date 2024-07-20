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


module Godot.Lang.Format where

import Godot.Lang.Core
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
import Data.Bool(bool)

import Godot.Lang.Core

-- Rendering of .gd files

addIndent :: String -> String
addIndent = unlines . fmap ("  " <>) . lines

fmtDefCls :: DefCls -> String
fmtDefCls (DefCls (ClsName cls) ext (DefClsInn enms conEnm _ defConsts defConVars defVars defFuncs)) =
  [i|
class_name #{cls} extends #{if ext == ExtendsObject then "object" else "reference"}

#{addIndent $ fmtEnum ("Con", conEnm)   }
#{addIndent $ concat $ fmap fmtEnum $ toList enms   }
#{addIndent $ intercalate "\n" $ fmap fmtDefVar (defVars <> (concat $ snd <$> toList defConVars))  }
#{addIndent $ concat $ fmap fmtDefFunc defFuncs   } |]

fmtEnum :: (String, [EnumVal]) -> String
fmtEnum (enm, vals) = [i|enum #{enm} { #{intercalate ", " $ fmap evVal vals} } |]

fmtDefVar :: DefVar -> String
fmtDefVar (DefVar nm typ ) =
  [i|var #{fmtVarName nm}: #{fmtTyp typ}|]

fmtVarName (VarName nm) = nm

fmtDefFunc :: DefFunc -> String
fmtDefFunc (DefFunc isSt (FuncName nm) args outTyp vars stmts) =
  [i|#{bool "" "static " isSt}func #{nm}(#{fmtArgs args}) -> #{fmtTyp outTyp}:
#{addIndent $ concatMap fmtStmt stmts}|]

fmtStmt (StmtCallFunc (FuncName fn)) = fn <> "()"
fmtStmt (StmtIf e s) = [i|if #{fmtBoolExpr e}: #{fmtStmt s} |]
fmtStmt (StmtIfElse e s s') = [i|if #{fmtBoolExpr e}: #{fmtStmt s} else: #{fmtStmt s} |]
fmtStmt (StmtFor v l s) = [i|for #{fmtVarName v} in #{fmtRangeExpr l} else: #{fmtStmt s} |]
fmtStmt (StmtMatch e css) = [i|match #{fmtExpr e}:
#{addIndent $ concatMap (\(e',s) -> fmtExpr e' <>": "<> fmtStmt s <> "\n") css} |]
fmtStmt (StmtRet e) = [i|return #{fmtExpr e} |]

fmtBoolExpr :: Expr Bool -> String
fmtBoolExpr ExprTrue = "true"
fmtBoolExpr ExprFalse = "False"

fmtRangeExpr :: Expr Enumerable -> String
fmtRangeExpr (ExprRange s e d) = [i|range(#{show s}, #{show e}, #{show d})|]

fmtExpr :: Expr t -> String
fmtExpr ExprTrue = "true"
fmtExpr ExprFalse = "False"
fmtExpr (ExprRange s e d) = [i|range(#{show s}, #{show e}, #{show d})|]
fmtExpr (ExprRangeVar v) = fmtVarName v
fmtExpr (ExprStr s) = [i|"#{s}"|]
fmtExpr (ExprRaw s) = s

fmtArgs :: [DefVar] -> String
fmtArgs args = intercalate ", "  $ fmtDefVar <$> args


fmtTyp (TypPrim PTInt    ) = "int"
fmtTyp (TypPrim PTFloat  ) = "flaot"
fmtTyp (TypPrim PTString ) = "String"
fmtTyp (TypPrim PTBool   ) = "bool"
fmtTyp (TypPrim PTV2     ) = "Vector2"
fmtTyp (TypPrim PTV3     ) = "Vector3"
fmtTyp (TypPrim PTArr    ) = "Array"
fmtTyp (TypPrim PTByteArr) = "PackedByteArray"
fmtTyp (TypCls (ClsName nm)) = nm
fmtTyp t = show t
