{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Data.Map.Strict (toList)
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
  | isEnum dc = fmtEnum (cls, fst <$> _dciDefConVars)
  | otherwise = [i|class #{cls} extends #{if ext == ExtendsObject then "Object" else "Reference"}:

|]
  <> breakLines (catMaybes
  [ if null _dciDefClasses then Nothing else Just $ addIndent (breakLines $ fmap fmtDefCls _dciDefClasses  )
  , if null enums then Nothing else Just $ addIndent (breakLines $ fmap fmtEnum enums)
  , if isSumType dc then Just $ addIndent "var con: Con" else Nothing
  , Just $ addIndent (breakLines $ fmap fmtDefStatVar _dciDefStatVars)
  , Just $ addIndent (breakLines $ fmap fmtDefVar (_dciDefVars <> concatMap snd _dciDefConVars))
  , Just $ addIndent (breakSpacedLines $ fmap fmtDefFunc _dciDefFuncs )
  ])
  where
    -- | Regular enums + special constructor enum "Con" in case it has more than 1 value
    enums :: [(String, [EnumVal])]
    enums = [("Con", fst <$> _dciDefConVars) | isSumType dc]
         <> toList _dciDefEnums

fmtEnum :: (String, [EnumVal]) -> String
fmtEnum (enm, vals) = [i|enum #{enm} { #{intercalate ", " $ fmap evVal vals} }|]

fmtDefStatVar :: DefVar -> String
fmtDefStatVar v = [i|static var #{fmtVar v}|]

fmtDefVar :: DefVar -> String
fmtDefVar v = [i|var #{fmtVar v}|]

fmtVar :: DefVar -> String
fmtVar (DefVar nm typ Nothing) = [i|#{fmtVarName nm}: #{fmtTyp typ}|]
fmtVar (DefVar nm typ (Just rawVal)) = [i|#{fmtVarName nm}: #{fmtTyp typ} = #{rawVal}|]

fmtVarName :: VarName -> String
fmtVarName (VarName nm) = nm

fmtDefFunc :: DefFunc -> String
fmtDefFunc (DefFunc isSt comm (FuncName nm) args outTyp _ stmts)
  = [i|# #{fromMaybe "TODO: Add comment" comm}
#{bool "" "static " isSt}func #{nm}(#{fmtArgs args}) -> #{fmtTyp outTyp}:
#{addIndent $ breakLines $ fmtStmt <$> stmts}|]

fmtStmt :: Stmt -> String
fmtStmt (StmtApp e) = fmtExpr e
fmtStmt (StmtIf e s) = [i|if #{fmtExpr e}: #{fmtStmt s} |]
fmtStmt (StmtIfElse e s s') = [i|if #{fmtExpr e}: #{fmtStmt s} else: #{fmtStmt s'} |]
fmtStmt (StmtFor v l s) = [i|
for #{fmtVarName v} in #{fmtExpr l}:
#{addIndent $ unlines $ fmtStmt <$> s} |]
fmtStmt (StmtMatch e (css, othMb))
  = [i|match #{fmtExpr e}:
|] <> addIndent (concatMap (\(e',ss) ->
  [i|#{fmtExpr e'}:
#{unlines (addIndent . fmtStmt  <$> ss)}|]) css)
   <> addIndent (maybe "" (\oth -> [i|_:#{unlines (addIndent . fmtStmt <$> oth)}|]) othMb)


fmtStmt (StmtRet e) = [i|return #{fmtExpr e} |]
fmtStmt (StmtVarInit v (Just e)) = [i|#{fmtDefVar v} = #{fmtExpr e} |]
fmtStmt (StmtVarInit v Nothing) = [i|#{fmtDefVar v}|]
fmtStmt (StmtSet (Iden id') e) = [i|#{intercalate "." id'} = #{fmtExpr e}|]
fmtStmt (StmtRaw s) = s

fmtExprElem :: ExprElem -> String
fmtExprElem (EElem e) = fmtExpr e

fmtExpr :: Expr t -> String
fmtExpr ETrue = "true"
fmtExpr EFalse = "False"
fmtExpr (ENot e) = "!(" <> fmtExpr e <> ")"
fmtExpr (EEq e0 e1) = fmtExpr e0  <> "==" <> fmtExpr e1
fmtExpr (EAnd es) = intercalate " && " $ fmap fmtExpr es
fmtExpr (EOr es) = "(" <> intercalate " || " (fmap fmtExpr es) <> ")"
fmtExpr (ERange s e d) = [i|range(#{show s}, #{show e}, #{show d})|]
fmtExpr (ERangeVar v) = fmtVarName v
fmtExpr (EStr s) = [i|"#{s}"|]
fmtExpr (EArr es) = [i|[ #{intercalate ", " (fmtExprElem <$> es)} ] |]
fmtExpr (EAt n e) = [i|#{fmtExpr e}[#{n}]|]
fmtExpr (ERaw s) = s
fmtExpr (EApp (FuncName fn) args) = fn <> "(" <> intercalate ", " (fmtExpr <$> args) <> ")"
fmtExpr (ELam vs e) = "func(" <> intercalate "," ((\(VarName vn) -> vn) <$> vs) <> "): return " <> fmtExpr e
fmtExpr (EId (Iden is)) = intercalate "." is

fmtArgs :: [DefVar] -> String
fmtArgs args = intercalate ", "  $ fmtVar <$> args

fmtTyp :: Typ -> String
fmtTyp = showTyp

