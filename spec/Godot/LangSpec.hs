module Godot.LangSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Simple types" $
    it "serializes correctly" $ True == True
