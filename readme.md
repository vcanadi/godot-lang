## godot-lang
Haskell library for converting Haskell types to Godot classes

### Utility
Generate corresponding Godot types(classes) from model written in Haskell. Serialization and deserialization is derived automatically, allowing interchange between isomorphic Godot and Haskell types.

### Usage

Haskell types with `ToDC` instances are collected and a script is generated with godot classes (or enums) that correspond to the Haskell types.
#### Generate script
```
toGDScriptExtra "gd-autogen" (Proxy @AllToDCInsts))
```
generates script `"common.gd"` in `"gd-autogen"` directory. `AllToDCInsts` is automatically generated type-level list of all types in scope with ToDC instance.
Execute from shell with `runhaskell -isrc src/Godot/Lang/Example/TH.hs`

#### From ghci
Alternatively DefCls can be generated with functions like `genDC` from ghci
e.g. `genDCs (Proxy @'[ CliMsg, SrvMsg, Maybe Dir  ])` generates types for CliMsg, SrvMsg and Maybe Dir types

### Examples
More detailed usage examples can be found in Example directory and in demo project.
Try changing the model in CliMsg and run `runhaskell -isrc src/Godot/Lang/Example/TH.hs` to see the diff in `gd-autogen/common.gd`

### How it works

#### Conversion description
Sum type is converted to class with a special enum `Con` (selection of the constructor) and variables for each of the constructor's parameters.
Sum type without parameters is converted to enum.
Regular (single constructor) types are converted to corresponding class

#### Output type
godot-lang defines a simple DSL that describes basic godot's syntax for defining classes and their elements (variables, subclasses, functions etc.)
Main type is `DefCls` (definition of the class).

#### Translation
Algorithm works on Haskell type's generic representation (GHC.Generics) and builds up a `DefCls`. Additional functions are added

#### Utilization of godot-ser
Algorithm adds additional functions for testing equality of two gd classes (Eq in Haskell), string represenatation (Show in haskell) and serialization and deserialization functions.
Ser/des functions are aligned with `godot-ser` library so that generated godot classes default binary serialization works with corresponding types.

#### Extra functions
Extra functions and static variables can be appended to some class via ToDC's optional functions (see in Example.SrvMsg)
