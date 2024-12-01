{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.TH where

import Language.Haskell.TH
import Godot.Lang.Example.CliMsg ()
import Godot.Lang.Example.SrvMsg ()
import Godot.Lang.Class (ToDC)
import Data.Maybe (mapMaybe)

-- get a list of instances
getToDCInstTypes :: Q [Type]
getToDCInstTypes = do
  ClassI _ insts <- reify ''ToDC
  let extractType (InstanceD _ _ (AppT _ t) _) = Just t
      extractType _                            = Nothing
  pure $ mapMaybe extractType insts

-- | Declaration: 'newtype AllToDCInsts = [...type level list of all ToDC instances...]'
allToDCInsts :: Q [Dec]
allToDCInsts = do
  types <-  getToDCInstTypes
  -- let types = [ConT $ mkName "X", ConT $ mkName "Y"]
  pure [TySynD (mkName "AllToDCInsts") [] $ foldr (AppT . AppT PromotedConsT) PromotedNilT types]
