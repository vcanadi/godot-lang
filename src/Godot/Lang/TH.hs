{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

{- | Automatic code generations based on ToDC instances
-}

module Godot.Lang.TH where

import Language.Haskell.TH (Q,Dec(InstanceD, TySynD), Info(ClassI), mkName, reify, runIO , Type(AppT, PromotedConsT, PromotedNilT))
import Godot.Lang.Class (ToDC, toGDScriptExtra, ToDCsExtra)
import Data.Maybe (mapMaybe)
import Data.Proxy

-- | Get a list of ToDC instances
qGetToDCInstTypes :: Q [Type]
qGetToDCInstTypes = do
  ClassI _ insts <- reify ''ToDC
  let extractType (InstanceD _ _ (AppT _ t) _) = Just t
      extractType _                            = Nothing
  pure $ mapMaybe extractType insts

-- | Declaration: 'newtype AllToDCInsts = [...type level list of all ToDC instances...]'
qAllToDCInsts :: Q [Dec]
qAllToDCInsts = do
  types <-  qGetToDCInstTypes
  pure [TySynD (mkName "AllToDCInsts") [] $ foldr (AppT . AppT PromotedConsT) PromotedNilT types]

qRunToGDScriptExtra :: forall as. ToDCsExtra as => Proxy as -> Q [Dec]
qRunToGDScriptExtra _ = runIO (toGDScriptExtra "." (Proxy @as)) >> pure []
