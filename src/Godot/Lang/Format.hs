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
import Control.Monad.Reader (Reader)

-- Rendering of .gd files

addIndent :: String -> String
addIndent = intercalate "\n" . fmap ("  " <>) . lines

newtype FmtState = FmtState { fsIndent :: Int }

type FmtR = Reader FmtState

-- | Separate with a newline
breakLines :: [String] -> String
breakLines = intercalate "\n"

-- | Separate with a newline and extra empty newline
breakSpacedLines :: [String] -> String
breakSpacedLines = intercalate "\n\n"


fmtDefCls :: DefCls -> String
fmtDefCls (DefCls (ClsName cls) ext (DefClsInn enms _ defConsts defConVars defVars defFuncs)) =
  [i|class_name #{cls} extends #{if ext == ExtendsObject then "object" else "reference"}

#{addIndent $ breakLines $ fmap fmtEnum $ ("Con", fst <$> toList defConVars) : toList enms   }

#{addIndent $ breakLines $ fmap fmtDefVar (defVars <> (concat $ snd <$> toList defConVars))  }

#{addIndent $ breakSpacedLines $ fmap fmtDefFunc defFuncs   } |]

fmtEnum :: (String, [EnumVal]) -> String
fmtEnum (enm, vals) = [i|enum #{enm} { #{intercalate ", " $ fmap evVal vals} }|]

fmtDefVar :: DefVar -> String
fmtDefVar (DefVar nm typ ) =
  [i|var #{fmtVarName nm}: #{fmtTyp typ}|]

fmtVarName (VarName nm) = nm

fmtDefFunc :: DefFunc -> String
fmtDefFunc (DefFunc isSt (FuncName nm) args outTyp vars stmts) =
  [i|#{bool "" "static " isSt}func #{nm}(#{fmtArgs args}) -> #{fmtTyp outTyp}:
#{addIndent $ breakLines $ fmtStmt <$> stmts}|]

fmtStmt (StmtCallFunc (FuncName fn)) = fn <> "()"
fmtStmt (StmtIf e s) = [i|if #{fmtBoolExpr e}: #{fmtStmt s} |]
fmtStmt (StmtIfElse e s s') = [i|if #{fmtBoolExpr e}: #{fmtStmt s} else: #{fmtStmt s} |]
fmtStmt (StmtFor v l s) = [i|for #{fmtVarName v} in #{fmtRangeExpr l} else: #{fmtStmt s} |]
fmtStmt (StmtMatch e css) = [i|match #{fmtExpr e}:
#{addIndent $ concatMap (\(e',s) -> fmtExpr e' <>": "<> fmtStmt s <> "\n") css} |]
fmtStmt (StmtRet e) = [i|return #{fmtExpr e} |]
fmtStmt (StmtVarInit v (Just e)) = [i|#{fmtDefVar v} = #{fmtExpr e} |]
fmtStmt (StmtVarInit v Nothing) = [i|#{fmtDefVar v}|]
fmtStmt (StmtSet vns e) = [i|#{intercalate "." $ vnName <$> vns} = #{fmtExpr e}|]

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
fmtExpr (ExprArr es) = [i| [ #{intercalate ", " (fmtExpr <$> es)} ] |]
fmtExpr (ExprRaw s) = s

fmtArgs :: [DefVar] -> String
fmtArgs args = intercalate ", "  $ fmtDefVar <$> args

fmtTyp :: Typ -> String
fmtTyp (TypPrim PTInt    ) = "int"
fmtTyp (TypPrim PTFloat  ) = "float"
fmtTyp (TypPrim PTString ) = "String"
fmtTyp (TypPrim PTBool   ) = "bool"
fmtTyp (TypPrim PTV2     ) = "Vector2"
fmtTyp (TypPrim PTV3     ) = "Vector3"
fmtTyp (TypPrim PTArr    ) = "Array"
fmtTyp (TypPrim PTByteArr) = "PackedByteArray"
fmtTyp (TypCls (ClsName nm)) = nm
fmtTyp (TypEnum enm) = enm
