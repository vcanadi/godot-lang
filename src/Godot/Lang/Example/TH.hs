{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.TH where

import Language.Haskell.TH
import Godot.Lang.Example.CliMsg ()
import Godot.Lang.Example.SrvMsg ()
import Godot.Lang.Class (ToDefCls)
import Data.Maybe (mapMaybe)

-- get a list of instances
getToDefClsInstTypes :: Q [Type]
getToDefClsInstTypes = do
  ClassI _ insts <- reify ''ToDefCls
  let extractType (InstanceD _ _ (AppT _ t) _) = Just t
      extractType _                            = Nothing
  pure $ mapMaybe extractType insts

-- | Declaration: 'newtype AllToDefClsInsts = [...type level list of all ToDefCls instances...]'
allToDefClsInsts :: Q [Dec]
allToDefClsInsts = do
  types <-  getToDefClsInstTypes
  -- let types = [ConT $ mkName "X", ConT $ mkName "Y"]
  pure [TySynD (mkName "AllToDefClsInsts") [] $ foldr (AppT . AppT PromotedConsT) PromotedNilT types]
