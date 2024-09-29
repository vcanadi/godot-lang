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
{-# LANGUAGE RecordWildCards #-}

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
import Data.Maybe (fromMaybe, catMaybes)

-- Rendering of .gd files

addIndent :: String -> String
addIndent = unlines . fmap ("  " <>) . lines

newtype FmtState = FmtState { fsIndent :: Int }

-- | Separate with a newline
breakLines :: [String] -> String
breakLines = intercalate "\n"

-- | Separate with a newline and extra empty newline
breakSpacedLines :: [String] -> String
breakSpacedLines = intercalate "\n\n"

fmtDefCls :: DefCls -> String
fmtDefCls dc@(DefCls (ClsName cls) ext DefClsInn{..})
  = [i|class #{cls} extends #{if ext == ExtendsObject then "Object" else "Reference"}:

|]
  <> breakLines (catMaybes
  [ if null _dciDefClasses then Nothing else Just $ addIndent (breakLines $ fmap fmtDefCls _dciDefClasses  )
  , if null enums then Nothing else Just $ addIndent (breakLines $ fmap fmtEnum enums)
  , if isSumType dc then Just $ addIndent "var con: Con" else Nothing
  , Just $ addIndent (breakLines $ fmap fmtDefVar (_dciDefVars <> concatMap snd (toList _dciDefConVars)))
  , Just $ addIndent (breakSpacedLines $ fmap fmtDefFunc _dciDefFuncs )
  ])
  where
    -- | Regular enums + special constructor enum "Con" in case it has more than 1 value
    enums :: [(String, [EnumVal])]
    enums = [("Con", M.keys _dciDefConVars) | isSumType dc]
         <> toList _dciDefEnums

fmtEnum :: (String, [EnumVal]) -> String
fmtEnum (enm, vals) = [i|enum #{enm} { #{intercalate ", " $ fmap evVal vals} }|]

fmtDefVar :: DefVar -> String
fmtDefVar v = [i|var #{fmtVar v}|]

fmtVar :: DefVar -> String
fmtVar (DefVar nm typ ) = [i|#{fmtVarName nm}: #{fmtTyp typ}|]

fmtVarName (VarName nm) = nm

fmtDefFunc :: DefFunc -> String
fmtDefFunc (DefFunc isSt comm (FuncName nm) args outTyp vars stmts)
  = [i|# #{fromMaybe "TODO: Add comment" comm}
#{bool "" "static " isSt}func #{nm}(#{fmtArgs args}) -> #{fmtTyp outTyp}:
#{addIndent $ breakLines $ fmtStmt <$> stmts}|]

fmtStmt (StmtApp e) = fmtExpr e
fmtStmt (StmtIf e s) = [i|if #{fmtBoolExpr e}: #{fmtStmt s} |]
fmtStmt (StmtIfElse e s s') = [i|if #{fmtBoolExpr e}: #{fmtStmt s} else: #{fmtStmt s} |]
fmtStmt (StmtFor v l s) = [i|
for #{fmtVarName v} in #{fmtRangeExpr l}:
#{addIndent $ unlines $ fmtStmt <$> s} |]
fmtStmt (StmtMatch e (css, othMb))
  = (<>) [i|match #{fmtExpr e}:|] $ addIndent $ (`concatMap` css) $ \(e',ss) -> "\n" <>
    [i|#{fmtExpr e'}:\n#{unlines (addIndent . fmtStmt  <$> ss)}|] <>
    maybe "" (\oth -> [i|_:\n#{unlines (addIndent . fmtStmt <$> oth)}|]) othMb


fmtStmt (StmtRet e) = [i|return #{fmtExpr e} |]
fmtStmt (StmtVarInit v (Just e)) = [i|#{fmtDefVar v} = #{fmtExpr e} |]
fmtStmt (StmtVarInit v Nothing) = [i|#{fmtDefVar v}|]
fmtStmt (StmtSet (Iden id') e) = [i|#{intercalate "." id'} = #{fmtExpr e}|]

fmtBoolExpr :: Expr Bool -> String
fmtBoolExpr ExprTrue = "true"
fmtBoolExpr ExprFalse = "False"

fmtRangeExpr :: Expr Enumerable -> String
fmtRangeExpr (ExprRange s e d) = [i|range(#{show s}, #{show e}, #{show d})|]
fmtRangeExpr (ExprRangeVar (VarName v)) = [i|#{v}|]

fmtExprElem :: ExprElem -> String
fmtExprElem (ExprElem e) = fmtExpr e

fmtExpr :: Expr t -> String
fmtExpr ExprTrue = "true"
fmtExpr ExprFalse = "False"
fmtExpr (ExprRange s e d) = [i|range(#{show s}, #{show e}, #{show d})|]
fmtExpr (ExprRangeVar v) = fmtVarName v
fmtExpr (ExprStr s) = [i|"#{s}"|]
fmtExpr (ExprArr es) = [i| [ #{intercalate ", " (fmtExprElem <$> es)} ] |]
fmtExpr (ExprRaw s) = s
fmtExpr (ExprApp (FuncName fn) args) = fn <> "(" <> intercalate ", " (fmtExpr <$> args) <> ")"
fmtExpr (ExprLam (VarName vn) e) = "func(" <> vn <> "): return " <> fmtExpr e
fmtExpr (ExprAny e) = fmtExpr e

fmtArgs :: [DefVar] -> String
fmtArgs args = intercalate ", "  $ fmtVar <$> args

fmtTyp :: Typ -> String
fmtTyp = showTyp

