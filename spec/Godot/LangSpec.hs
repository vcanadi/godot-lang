module Godot.LangSpec where

import Test.Hspec
import Test.Hspec.Golden
import Godot.Lang.Example.TH
import Godot.Lang.Class
import Data.Proxy

spec :: Spec
spec = do
  describe "Simple types" $
    it "serializes correctly" $ True == True

  describe "toGDTextExtra" $
    it "generates the right gd script" $
       let output = toGDTextExtra (Proxy @AllToDCInsts)
         in defaultGolden "toGDTextExtra" output
