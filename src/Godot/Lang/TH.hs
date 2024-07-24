{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.TH where

import Godot.Lang.Trans

main :: IO ()
main = do
    let compileTimeString = $(generateGDScript)
    putStrLn compileTimeString
