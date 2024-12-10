{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Godot.Lang.Example.TH where

import Godot.Lang.Example.Common ()
import Godot.Lang.Example.CliMsg ()
import Godot.Lang.Example.SrvMsg ()
import Godot.Lang.TH
-- import Godot.Lang.Class (toGDScriptExtra)
-- import Data.Proxy (Proxy(Proxy))
-- import Language.Haskell.TH

-- Add any addition instances Of ToDC

-- | Paste here declaration 'newtype AllToDCInsts = [...type level list of all ToDC instances...]'
$(qAllToDCInsts)

-- | Example how to run toGDScriptExtra at compiletime to generate ./common.gd file
-- $(runIO (toGDScriptExtra "." (Proxy @AllToDCInsts)) >> pure [])
