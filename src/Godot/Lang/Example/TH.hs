{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.TH where

import Godot.Lang.Example.CliMsg

main :: IO ()
main = do
    let compileTimeString = $(generateGDScript)
    putStrLn compileTimeString
