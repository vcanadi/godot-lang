{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- | Functions added to DefCls (e.g. for converting to string or serialization functions)
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
import Data.List (intercalate)
import Data.Char (toLower, chr, ord)
import Data.Maybe (maybeToList)
import Control.Monad (join)
import Godot.Lang.Core

exprCon con = ExprRaw $ "Con." <> con

addBasicFunctions :: DefCls -> DefCls
addBasicFunctions = addSerialization
                  . addDeserialization
                  . addConShow
                  . addCons

-- | Enrich DefCls with "constructor" functions for each constructor in sum type
addCons :: DefCls -> DefCls
addCons dc@DefCls{..} = dc & dcInn . dciDefFuncs %~ (<> fs)
                           & dcInn . dciDefClasses %~ fmap addCons
  where
    fs =
      [ defStatFunc ("Constructor function for sum constructor " <> con)
          (toLower <$> con) (join $ maybeToList $ M.lookup (EnumVal con) $ _dciDefConVars _dcInn) (TypCls _dcName ) [] $
          [ ("ret" -:: TypCls _dcName) -:= ExprRaw (cnName _dcName <> ".new()")
          , ["ret", "con"] --= exprCon con ] <>
          [ ["ret",  vn] --= ExprRaw vn | DefVar (VarName vn) _ <- vs ] <>
          [ StmtRet (ExprRaw "ret") ]
      | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn
      ]

-- | Enrich DefCls  with show function (shows only constructor name)
addConShow :: DefCls -> DefCls
addConShow dc = dc & dcInn . dciDefFuncs %~ (<> [f])
                   & dcInn . dciDefClasses %~ fmap addConShow
  where
    f = DefFunc False (Just "String representation of type") (FuncName "show") [] (TypPrim PTString) []
      [ StmtMatch (ExprRaw "self.con")
          ( [(exprCon con , [StmtRet $ ExprStr con])
            | (EnumVal con,_) <- toList $ _dciDefConVars $ _dcInn dc
            ]
          , Just [StmtRet $ ExprRaw "\"\""] )
      ]



-- Serializing expressions
--
-- | Enrich DefCls with serialization function compatible with godot-ser serialization
addSerialization :: DefCls -> DefCls
addSerialization dc@DefCls{..} = dc & dcInn . dciDefFuncs %~ (<> fs)
                                    & dcInn . dciDefClasses %~ fmap addSerialization
  where
    fs =
      [ defStatFunc "Serialize to array"
          "serToArr" ["this" -:: TypCls _dcName ] (TypArr TypAny) []
          [ StmtMatch (ExprRaw "this.con")
              ( [ (exprCon con , [ StmtRet $ exprClsSer con vs])
                | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn
                ]
              , Just [StmtRet $ ExprRaw "[]"])
          ]
      , defStatFunc "Serialize to binary"
          "ser" ["this" -:: TypCls _dcName] (TypPrim PTByteArr) []
          [StmtRet $ ExprRaw "var_to_bytes(serToArr(this))"]
      ]

-- | gd expression that serializes class based on selected constructor
exprClsSer :: String -> [DefVar] -> Expr Arr
exprClsSer con vs = ExprArr $ [ ExprElem $ exprCon con ] <> [ ExprElem $ exprValueSer vn typ | (DefVar (VarName vn) typ) <- vs ]

-- | gd expression that serializes value
exprValueSer vn typ = case typ of
  TypCls (ClsName cn) -> ExprAny $ ExprRaw $ cn <> ".serToArr(this." <> vn <> ")"
  TypArr typ' -> ExprAny $ (vn <> ".map") --$ ["x" --> exprValueSer "x" typ']
  TypPair a b -> ExprAny $ ExprArr [ ExprElem $ exprValueSer (vn <> ".fst") a
                                   , ExprElem $  exprValueSer (vn <> ".snd") b
                                   ]
  TypDict a b -> ExprAny $ (vn <> ".keys().map") --$ ["k" --> ExprArr [ ExprElem $ exprValueSer "k" a
                                                                      , ExprElem $  exprValueSer (vn <> "[k]") b
                                                                      ]
                                                     ]
  TypEnum _    -> ExprAny (ExprRaw vn)
  TypPrim _    -> ExprAny (ExprRaw vn)

-- Deserializing expressions
--
-- | Enrich DefCls with deserialization function compatible with godot-ser serialization
addDeserialization :: DefCls -> DefCls
addDeserialization dc@DefCls{..} =dc & dcInn . dciDefFuncs %~ (<> fs)
                                     & dcInn . dciDefClasses %~ fmap addDeserialization
  where
    fs =
     [ defStatFunc "Deserialize from array"
          "desFromArr" [ "arr" -:: TypArr TypAny ] (TypCls _dcName) []
          [ ("ret" -:: TypCls _dcName) -:= ExprRaw (cnName _dcName <> ".new()")
          , ["ret", "con"] --= ExprRaw "arr[0]"
          , StmtMatch (ExprRaw "ret.con")
             ( [ (exprCon con, stmtsClsDes con vs)
               | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn, not $ null vs
               ]
             , Nothing)
          , StmtRet (ExprRaw "ret")
          ]

      , defStatFunc "Deserialize from binary"
          "des" ["this" -:: TypPrim PTByteArr] (TypCls _dcName) []
          [StmtRet $ ExprRaw "desFromArr(bytes_to_var(this))"]
      ]

-- | gd statements that deserialize class based on selected constructor
stmtsClsDes :: p -> [DefVar] -> [Stmt]
stmtsClsDes con vs = concat
  [  stmtsValueDes ("arr[" <> show i <> "]") ("ret." <> vn) 0 typ
  -- <> [["ret", vn] --= ExprRaw vn]
  | (i, DefVar (VarName vn) typ) <- zip [1..] vs
  ]

ixVar :: String -> Int -> String
ixVar s n = s <> ['_',chr $ ord 'A' + n]

stmtsValueDes :: String -> String -> Int -> Typ -> [Stmt]
stmtsValueDes nmIn nmOut i typ = case typ of
  TypCls (ClsName nm) -> [ [nmOut] --= ExprAny (ExprRaw $ nm <> ".desFromArr(" <> nmIn <> ")") ]
  TypArr typ' -> [ StmtApp $ (nmOut <> ".assign") --$ [(nmIn <> ".map") --$ ["x" --> exprArrDes nmIn typ']]]
  TypDict a b -> [ ixVar "dict" i -:: TypDict a b -:= ExprRaw "{}"
                 , StmtVarInit (ixVar "k" i -:: a) Nothing
                 , StmtVarInit (ixVar "v" i -:: b) Nothing
                 , StmtFor (VarName $ ixVar "pair" i) (ExprRangeVar $ VarName nmIn) $
                        stmtsValueDes (ixVar "pair" i <> "[0]") (ixVar "k" i) (succ i) a
                     <> stmtsValueDes (ixVar "pair" i <> "[1]") (ixVar "v" i) (succ i) b
                     <> [ [ixVar "dict" i <> "["<> ixVar "k" i <> "]"] --= ExprRaw (ixVar "v" i)
                     ]
                 , [nmOut] --= ExprRaw (ixVar "dict" i)
                 ]
  -- TypPair a b -> [ ["ret", nmIn] --= ExprRaw ("arr[" <> show i <> "]") ]
  TypEnum _   -> [ [nmOut] --= ExprRaw nmIn ]
  __          -> [ [nmOut] --= ExprRaw nmIn ]

-- | gd expression that deserializes arrays
exprArrDes nm (TypArr t')           = ExprAny $ (nm <> ".map") --$ ["x" --> exprArrDes "x" t']
exprArrDes nm (TypCls (ClsName cn)) = ExprAny $ ExprRaw "desFromArr"
exprArrDes nm (TypPair a b)         = ExprAny $ ExprRaw $ "[" <> nm <> "[0], " <> nm <> "[1]]"
exprArrDes nm _                     = ExprAny $ ExprRaw nm

