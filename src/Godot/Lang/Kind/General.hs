{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , GADTs
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
#-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}


module Godot.Lang.Kind.General where

import Data.Text (Text)

import GHC.TypeLits (Symbol, AppendSymbol)
import GHC.Base (Type)

-- Step 4: Use Template Haskell to Generate Singletons
-- $(singletons [d|
--   data Nat = Zero | Succ Nat
--     deriving (Show)

--   add :: Nat -> Nat -> Nat
--   add Zero m = m
--   add (Succ n) m = Succ (add n m)

--   f :: Nat -> Nat
--   f a = Succ a

--   |])

-- Step 5: Use the Generated Type-Level Functions
-- example1 :: Sing ('Add 'Zero ('Succ 'Zero))
-- example1 = SSucc SZero

-- example2 :: Sing ('Add ('Succ 'Zero) ('Succ 'Zero))
-- example2 = SSucc (SSucc SZero)

-- main :: IO ()
-- main = do
--   putStrLn $ "Proof that Add Zero (Succ Zero) is Succ Zero: " ++ show example1
--   putStrLn $ "Proof that Add (Succ Zero) (Succ Zero) is Succ (Succ Zero): " ++ show example2
--


-- data Nat = Zero | Succ Nat
--   deriving (Show)

-- type family Fmap (f :: i -> j) (as :: [i]) = (bs :: [j]) | bs -> f where
--   Fmap f '[a]           = '[f a]
--   Fmap f (a ': b ': as) =  f a ': Fmap f (b ': as)

-- type family Foldl (f :: j -> i -> j) (b :: j) (as :: [i]) = (r :: j) where
--   Foldl _ b '[] = b
--   Foldl f b  (a ': as) = Foldl f (f b a) as

-- type family ConcatMap (f :: i -> Symbol) (as :: [i]) = (b :: Symbol) where
--   ConcatMap _ '[] = ""
--   ConcatMap f (a ': as) = AppendSymbol (f a) (ConcatMap f as)

-- type family F t where
--   F Int = "an int"
--   F a = "some type"

