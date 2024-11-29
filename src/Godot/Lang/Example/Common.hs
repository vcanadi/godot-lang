{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Godot.Lang.Example.Common where

import Godot.Lang.Trans
import Language.Haskell.TH (Q, Exp, runIO)
import Godot.Lang.Example.CliMsg
import Godot.Lang.Example.SrvMsg


generateGDScript :: Q Exp
generateGDScript = do
    runIO $ genGDScript' @[ CliMsg, SrvMsg, Dir] "./gd-autogen"
    [| "This is generated at compile-time." |]
