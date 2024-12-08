{-# LANGUAGE TemplateHaskell #-}

{- | Automatic code generations based on ToDC instances
-}

module Godot.Lang.TH where

import Language.Haskell.TH
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
  pure [TySynD (mkName "AllToDCInsts") [] $ foldr (AppT . AppT PromotedConsT) PromotedNilT types]
