{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Data.String.Interpolate (i)


-- perTypDefFunc dc comm fn args retTyp =
--   comm ###
--   func fn args retTyp (if isSumType dc then sumTypeMatch else prodTypeCase)
--   where
--     sumTypeMatch =
--         [ StmtMatch (ERaw "self.con")
--             ( [(eCon con , [StmtRet $ EStr con])
--               | (EnumVal con,_) <- toList $ _dciDefConVars _dcInn
--               ]
--             , Just [StmtRet $ ERaw "\"\""] )
--         ]
--     prodTypeCase =
--         [StmtRet $ ERaw con
--         | (EnumVal con, vs) <- toList $ _dciDefConVars _dcInn
--         ]

addBasicFunctions :: DefCls -> DefCls
addBasicFunctions = addFuncsRecursive addSerToArr
                  . addFuncsRecursive addSerToBin
                  . addFuncsRecursive addDesFromArr
                  . addFuncsRecursive addDesFromBin
                  . addFuncsRecursive addConShow
                  . addFuncsRecursive addCons
                  . addFuncsRecursive addEq

-- | Enrich DefCls with "constructor" functions for each constructor in sum type
addCons :: DefCls -> [DefFunc]
addCons dc@DefCls{..} =
  [ [i|Constructor function for sum constructor #{con}|] ###
    stat_func (toLower <$> con) (join $ maybeToList $ lookup (EnumVal con) $ _dciDefConVars _dcInn) (TypCls _dcName )
      ( [ ("ret" -:: TypCls _dcName) -:= ERaw (cnName _dcName <> ".new()") ] <>
        [ ["ret", "con"] --= eCon con  | isSumType dc] <>
        [ ["ret",  vn] --= ERaw vn
          | DefVar (VarName vn) _ <- vs
        ] <>
        [ StmtRet (ERaw "ret") ]
      )
    | (EnumVal con, vs) <- _dciDefConVars _dcInn
  ]

-- | Enrich DefCls  with show function (shows only constructor name)
addConShow :: DefCls -> [DefFunc]
addConShow dc@DefCls{..} =
  [ [i|String representation of type|] ###
    func "show" [] (TypPrim PTString)
      (if isSumType dc then sumTypeMatch else prodTypeCase)
  ]
  where
    sumTypeMatch =
        [ StmtMatch (ERaw "self.con")
            ( [(eCon con , [StmtRet $ EStr con])
              | (EnumVal con,_) <- _dciDefConVars _dcInn
              ]
            , Just [StmtRet $ EStr ""] )
        ]
    prodTypeCase =
        [StmtRet $ EStr con
        | (EnumVal con, vs) <- _dciDefConVars _dcInn
        ]


-- | Eq instances
addEq :: DefCls -> [DefFunc]
addEq dc@DefCls{..} =
  [ [i| Equality check of two #{cnName _dcName} |] ###
    stat_func "eq" ["a" -:: TypCls _dcName, "b" -:: TypCls _dcName] (TypPrim PTBool)
      (if isSumType dc then sumTypeCase else prodTypeCase)

  , [i| Non-static equality check of two #{cnName _dcName} |] ###
    func "eq1" ["b" -:: TypCls _dcName] (TypPrim PTBool) [StmtRet $ ERaw $ cnName _dcName <> ".eq(self, b)"]
  ]
  where
    sumTypeCase =
      [ StmtRet $
         eId ["a","con"] -== eId ["b","con"]
         -&& EOr [ (eId ["a", "con"] -== eCon con) -&& exprClsEq vs
                 | (EnumVal con,vs) <- _dciDefConVars _dcInn
                 ]
      ]
    prodTypeCase =
      [ StmtRet $ exprClsEq vs
      | (EnumVal con, vs) <- _dciDefConVars _dcInn
      ]

    -- | gd expression that tests equality based on selected constructor(optionally)
    exprClsEq :: [DefVar] -> Expr Bool
    exprClsEq vs = EAnd [ exprValueEq ("a." <> vn) ("b." <> vn) typ | (DefVar (VarName vn) typ) <- vs ]


    -- | gd expression that serializes value
    exprValueEq ::  String -> String -> Typ -> Expr Bool
    exprValueEq aVn bVn typ = case typ of
      TypCls (ClsName cn) -> ERaw $ cn <> ".eq(" <> aVn <> ", " <> bVn <> ")"
      TypArr typ' -> EAt 0 ((aVn <> ".reduce") --$ [ELam [VarName "acc", VarName "x"] $ EArr [EElem $ EAnd [ERaw "acc[0]" , exprValueEq "x" (bVn <> "[acc[1]]") typ'], EElem $ ERaw "acc[1] + 1"], ERaw "[true, 0]"  ])
      TypPair a b -> ERaw $ showTyp (TypPair a b) <> ".eq(" <> aVn <> ", " <> bVn <> ")"
      -- TODO
      -- TypDict a b -> (aVn <> ".keys().map") --$ ["k" --> EArr [ EElem $ exprValueEq "k" a
      --                                                                     , EElem $  exprValueEq (vn <> "[k]") b
      --                                                                     ]
      --                                                    ]
      _     -> EEq (ERaw aVn) (ERaw bVn)

-- Serializing expressions
--
-- | Enrich DefCls with array serialization function compatible with godot-ser serialization
addSerToArr :: DefCls -> [DefFunc]
addSerToArr dc@DefCls{..} =
  [ "Serialize to array" ###
    stat_func "serToArr" ["this" -:: TypCls _dcName ] (TypArr TypAny)
      (if isSumType dc then sumTypeMatch else prodTypeCase)
  ]
  where
    sumTypeMatch =
      [ StmtMatch (ERaw "this.con")
          ( [ (eCon con , [ StmtRet $ exprClsSer (Just con) vs])
            | (EnumVal con, vs) <- _dciDefConVars _dcInn
            ]
          , Just [StmtRet $ ERaw "[]"])
      ]
    prodTypeCase =
      [ StmtRet $ exprClsSer Nothing vs
      | (EnumVal con, vs) <- _dciDefConVars _dcInn
      ]

    -- | gd expression that serializes class based on selected constructor(optionally)
    exprClsSer conMb vs = case conMb of
      -- | No constructor (product type)
      Nothing -> case vs of
        -- | Single product argument (newtype or data)
        [DefVar (VarName vn) typ] -> exprValueSer ("this." <> vn) typ
        -- | Multiple product arguments
        _                         -> EArr [ EElem $ exprValueSer ("this." <> vn) typ | (DefVar (VarName vn) typ) <- vs ]
      -- | Existing constructor (sum type)
      Just con -> EArr $ [EElem $ eCon con] <> [ EElem $ exprValueSer ("this." <> vn) typ | (DefVar (VarName vn) typ) <- vs ]



    -- | gd expression that serializes value
    exprValueSer vn typ = case typ of
      TypCls (ClsName cn) ->  ERaw $ cn <> ".serToArr(" <> vn <> ")"
      TypArr typ' -> (vn <> ".map") --$ ["x" --> exprValueSer "x" typ']
      TypPair a b -> ERaw $ showTyp (TypPair a b) <> ".serToArr(" <>  vn <> ")"

      TypDict a b -> (vn <> ".keys().map") --$ ["k" --> EArr [ EElem $ exprValueSer "k" a
                                                             , EElem $  exprValueSer (vn <> "[k]") b
                                                             ]
                                                         ]
      TypEnum _    -> ERaw vn
      TypPrim _    -> ERaw vn

-- Deserializing expressions
--
-- | Enrich DefCls with deserialization function compatible with godot-ser serialization
addDesFromArr :: DefCls -> [DefFunc]
addDesFromArr dc@DefCls{..} =
  [ "Deserialize from array" ###
    stat_func "desFromArr" [ "arr" -:: TypArr TypAny ] (TypCls _dcName)
      ( [ ("ret" -:: TypCls _dcName) -:= ERaw (cnName _dcName <> ".new()")
        ] <> ( if isSumType dc then sumTypeMatch else prodTypeCase
        ) <> [ StmtRet (ERaw "ret") ]
      )
  ]
  where
    sumTypeMatch =
      [["ret", "con"] --= ERaw "arr[0]"
      , StmtMatch (ERaw "ret.con")
         ( [ (eCon con, stmtsClsDes 1 con vs)
           | (EnumVal con, vs) <- _dciDefConVars _dcInn, not $ null vs
           ]
         , Nothing)
      ]
    prodTypeCase =
      concat [ stmtsClsDes 0 con vs
      | (EnumVal con, vs) <- _dciDefConVars _dcInn, not $ null vs
      ]

    -- | gd statements that deserialize class based on selected constructor
    stmtsClsDes :: Int -> p -> [DefVar] -> [Stmt]
    stmtsClsDes start con vs = case vs of
      [DefVar (VarName vn) typ] -> stmtsValueDes "arr" ("ret." <> vn) 0 typ
      _                         -> concat
        [  stmtsValueDes ("arr[" <> show i <> "]") ("ret." <> vn) 0 typ
        | (i, DefVar (VarName vn) typ) <- zip [start..] vs
        ]

    ixVar :: String -> Int -> String
    ixVar s n = s <> ['_',chr $ ord 'A' + n]

    stmtsValueDes :: String -> String -> Int -> Typ -> [Stmt]
    stmtsValueDes nmIn nmOut i typ = case typ of
      TypCls (ClsName nm) -> [ [nmOut] --= ERaw (nm <> ".desFromArr(" <> nmIn <> ")") ]
      TypArr (TypCls (ClsName nm)) -> [ StmtApp $ (nmOut <> ".assign") --$ [(nmIn <> ".map") --$ [ERaw $ nm <> ".desFromArr"]]]
      TypArr (TypPair a b) -> [ StmtApp $ (nmOut <> ".assign") --$ [(nmIn <> ".map") --$ [ERaw $ showTyp (TypPair a b) <> ".desFromArr"]]]
      TypArr typ' -> [ StmtApp $ (nmOut <> ".assign") --$ [(nmIn <> ".map") --$ ["x" --> exprArrDes "x" typ']]]
      TypDict a b -> [ ixVar "dict" i -:: TypDict a b -:= ERaw "{}"
                     , StmtVarInit (ixVar "k" i -:: a) Nothing
                     , StmtVarInit (ixVar "v" i -:: b) Nothing
                     , StmtFor (VarName $ ixVar "pair" i) (ERangeVar $ VarName nmIn) $
                            stmtsValueDes (ixVar "pair" i <> "[0]") (ixVar "k" i) (succ i) a
                         <> stmtsValueDes (ixVar "pair" i <> "[1]") (ixVar "v" i) (succ i) b
                         <> [ [ixVar "dict" i <> "["<> ixVar "k" i <> "]"] --= ERaw (ixVar "v" i)
                         ]
                     , [nmOut] --= ERaw (ixVar "dict" i)
                     ]
      TypPair a b -> [ [nmOut] --= ERaw ("arr[" <> show i <> "]") ]
      TypEnum _   -> [ [nmOut] --= ERaw nmIn ]
      __          -> [ [nmOut] --= ERaw nmIn ]

    -- | gd expression that deserializes arrays
    exprArrDes nm (TypArr t')           = (nm <> ".map") --$ ["x" --> exprArrDes "x" t']
    exprArrDes nm (TypCls (ClsName cn)) = ERaw "desFromArr"
    exprArrDes nm (TypPair a b)         = ERaw $ showTyp (TypPair a b) <> ".desFromArr(" <> nm <> ")"
    exprArrDes nm _                     = ERaw nm


-- Binary serialization/deserialization

-- | Enrich DefCls with binary serialization function compatible with godot-ser serialization
addSerToBin :: DefCls -> [DefFunc]
addSerToBin dc@DefCls{..} =
  [ "Serialize to binary" ###
    stat_func "ser" ["this" -:: TypCls _dcName] (TypPrim PTByteArr)
     [StmtRet $ ERaw "var_to_bytes(serToArr(this))"]
  ]

-- | Enrich DefCls with binary deserialization function compatible with godot-ser serialization
addDesFromBin :: DefCls -> [DefFunc]
addDesFromBin dc@DefCls{..} =
  [ "Deserialize from binary" ###
    stat_func "des" ["this" -:: TypPrim PTByteArr] (TypCls _dcName)
      [StmtRet $ ERaw "desFromArr(bytes_to_var(this))"]
  ]
