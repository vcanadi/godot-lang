{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- | Core module that defines structure of GD script language
-}
module Godot.Lang.Functions where

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
import Godot.Lang.Core

conExpr con = ExprRaw $ "Con." <> con

addBasicFunctions :: DefCls -> DefCls
addBasicFunctions = addSerialization
                  . addDeserialization
                  . addConShow
                  . addCons

-- | Enrich DefCls with "constructor" functions for each constructor in sum type
addCons :: DefCls -> DefCls
addCons dc@DefCls{..} = (`addDefFuncs` dc) $
  [ defStatFunc ("Constructor function for sum constructor " <> con)
      (toLower <$> con) (join $ maybeToList $ M.lookup (EnumVal con) $ _dciDefConVars _dcInn) (TypCls _dcName ) [] $
      [ ("ret" -:: TypCls _dcName) -:= ExprRaw (cnName _dcName <> ".new()")
      , ["ret", "con"] --= conExpr con ] <>
      [ ["ret",  vn] --= ExprRaw vn | DefVar (VarName vn) _ <- vs ] <>
      [ StmtRet (ExprRaw "ret") ]
  | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn
  ]

-- | Enrich DefCls  with show function (shows only constructor name)
addConShow :: DefCls -> DefCls
addConShow dc = (`addDefFunc` dc) $
  DefFunc False (Just "String representation of type") (FuncName "show") [] (TypPrim PTString) []
        [ StmtMatch (ExprRaw "self.con")
            [(conExpr con , [StmtRet $ ExprStr con])
            | (EnumVal con,_) <- toList $ _dciDefConVars $ _dcInn dc]
        ]


-- Serializing expressions
--
-- | Enrich DefCls with serialization function compatible with godot-ser serialization
addSerialization :: DefCls -> DefCls
addSerialization dc@DefCls{..} = (`addDefFuncs` dc)
  [ defStatFunc "Serialize to array"
      "serToArr" ["this" -:: TypCls _dcName ] (TypArr TypAny) []
      [ StmtMatch (ExprRaw "this.con")
          [ (conExpr con , [ StmtRet $ exprClsSer con vs])
          | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn
          ]
      ]
  , defStatFunc "Serialize to binary"
      "ser" ["this" -:: TypCls _dcName] (TypPrim PTByteArr) []
      [StmtRet $ ExprRaw "var_to_bytes(serToArr(this))"]
  ]

-- | gd expression that serializes class based on selected constructor
exprClsSer :: String -> [DefVar] -> Expr Arr
exprClsSer con vs =
  ExprArr $
    [ ExprElem $ conExpr con ] <>
    [ case typ of
        TypCls _    -> ExprElem $ ExprRaw $ vn <> ".serToArr()"
        TypArr typ' -> ExprElem $ exprArrSer vn typ'
        _           -> ExprElem (ExprRaw vn)
    | (DefVar (VarName vn) typ) <- vs
    ]

-- | gd expression that serializes arrays
exprArrSer nm (TypArr t')           = ExprAny $ (nm <> ".map") --$ ["x" --> exprArrSer "x" t']
exprArrSer nm (TypCls (ClsName cn)) = ExprAny $ (nm <> ".map") --$ [ExprRaw  "serToArr"]
exprArrSer nm _                     = ExprAny $ ExprRaw nm


-- Deserializing expressions
--
-- | Enrich DefCls with deserialization function compatible with godot-ser serialization
addDeserialization :: DefCls -> DefCls
addDeserialization dc@DefCls{..} = (`addDefFuncs` dc)
  [ defStatFunc "Deserialize from array"
      "desFromArr" [ "arr" -:: TypArr TypAny ] (TypCls _dcName) []
      [ ("ret" -:: TypCls _dcName) -:= ExprRaw (cnName _dcName <> ".new()")
      , ["ret", "con"] --= ExprRaw "arr[0]"
      , StmtMatch (ExprRaw "arr[0]")
         [ (conExpr con, stmtsClsDes con vs)
         | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn, not $ null vs
         ]
      , StmtRet (ExprRaw "ret")
      ]

  , defStatFunc "Deserialize from binary"
      "des" ["this" -:: TypPrim PTByteArr] (TypCls _dcName) []
      [StmtRet $ ExprRaw "desFromArr(bytes_to_var(bs))"]
  ]

-- | gd statements that deserialize class based on selected constructor
stmtsClsDes :: p -> [DefVar] -> [Stmt]
stmtsClsDes con vs = concat
  [ case typ of
      TypCls _    -> [ ["ret", vn] --= ExprRaw ("arr[" <> show i <> "].desFromArr()") ]
      TypArr typ' -> [ StmtApp $ ("ret." <>  vn <> ".assign") --$ [exprArrDes ("arr[" <> show i <> "]") typ']]
      -- TypDict k v -> [ StmtApp $ ("ret." <>  vn <> ".assign") --$ [exprArrDes ("arr[" <> show i <> "]") $ TypPair k v  ]]
      _   -> [ ["ret", vn] --= ExprRaw ("arr[" <> show i <> "]") ]
      -- TypEnum _   -> [ ["ret", vn] --= ExprRaw ("arr[" <> show i <> "]") ]

  | (i, DefVar (VarName vn) typ) <- zip [1..] vs
  ]


-- | gd expression that serializes arrays
exprArrDes nm (TypArr t')           = ExprAny $ (nm <> ".map") --$ ["x" --> exprArrDes "x" t']
exprArrDes nm (TypCls (ClsName cn)) = ExprAny $ (nm <> ".map") --$ [ExprRaw "desFromArr"]
exprArrDes nm _                     = ExprAny $ ExprRaw nm



