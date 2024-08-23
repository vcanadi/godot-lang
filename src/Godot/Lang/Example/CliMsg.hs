{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.CliMsg where

import Godot.Lang.Trans
import Linear.V2 (V2)
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Haskell.TH

data CliMsg
  = JOIN
  | LEAVE String
  | ACTION { act :: Action , time :: Int, vec :: V2 Double}
  | GET_LISTS { glList :: [Float], glList2 :: [Action] }
  | GET_MAP { gmMap :: Map Float String}
  deriving (Show, Generic)

data Action = MOVE Int
            | FIRE Int
  deriving (Show, Eq, Read, Generic)


generateGDScript :: Q Exp
generateGDScript = do
    runIO $ genGDScript @CliMsg "./gd-autogen"
    [| "This string is gen erated at compile-time." |]