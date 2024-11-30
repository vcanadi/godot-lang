{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Godot.Lang.Example.TH2 where

import Godot.Lang.Example.Common
import Godot.Lang.Example.CliMsg
import Godot.Lang.Example.SrvMsg
import Godot.Lang.Example.TH
import Godot.Lang.Trans
import Godot.Lang.Class (LUnion, ToDefCls)

-- Add any addition instances Of ToDefCls
instance ToDefCls (Maybe Dir)

$(allToDefClsInsts)


main :: IO ()
main = do
    putStrLn "bla"
