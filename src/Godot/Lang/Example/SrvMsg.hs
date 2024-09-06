{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.SrvMsg where

import Godot.Lang.Trans
import Linear.V2 (V2)
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Haskell.TH (Q, Exp, runIO)
import Godot.Lang.Core (ToTyp (toTyp), Typ (TypPrim), PrimTyp (PTString))

data Action = Action Model deriving (Show, Eq, Generic)

data SrvMsg = PUT_STATE Model [Action]
  deriving (Show, Eq, Generic)

-- | State of the game (client info and board coordinates)
type Model = Map String Loc

data Loc = Loc
  { _mX :: Int
  , _mY :: Int
  } deriving (Show, Eq, Generic)

generateGDScript :: Q Exp
generateGDScript = do
    runIO $ genGDScript @SrvMsg "./gd-autogen"
    [| "This string is generated at compile-time." |]

