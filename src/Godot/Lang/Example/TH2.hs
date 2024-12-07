{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Godot.Lang.Example.TH2 where

import Godot.Lang.Example.Common ()
import Godot.Lang.Example.CliMsg
import Godot.Lang.Example.SrvMsg ()
import Godot.Lang.Example.TH
import Godot.Lang.Class (ToDC)

-- Add any addition instances Of ToDC
instance ToDC (Maybe Dir)

$(allToDCInsts)