----------------------------------------------------------------------
-- Functional Dependencies examples and experimentation

-- Simple Haskell syntax example and play
{-# LANGUAGE UnicodeSyntax #-}
-- Extensions not included in Clash
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- The following allow us to load this in `stack exec ghci`:
{-# LANGUAGE GADTs, KindSignatures #-}

-- We turn this on and off as we experiment
{-# LANGUAGE AllowAmbiguousTypes #-}

module Hask.FDs where

import Prelude
import Data.Kind (Type)             -- also Clash.Prelude (Type)

class Coll σ ε where
    empty  ∷ σ
    insert ∷ σ → ε → σ

instance Coll [Int] Int where
    empty = []
    insert c e = e : c

instance Coll [Int] Integer where
    empty = []
    insert c e = fromIntegral e : c

instance Coll [Integer] Int where
    empty = []
    insert c e = fromIntegral e : c

instance Coll (Int,()) Int where
    empty = (0,())
    insert c e = (e,())

-- Fails even through only one `empty` has this type
-- c1a = empty :: [Integer]

-- Fails even though `empty` from two instances returns the same type.
-- c1b = empty :: [Int]

-- `TypeApplications` extension lets us specify the exact `empty` to use:
-- §6.4.14: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_applications.html
-- This dates from 8.0.1, much newer than `FunctionalDependencies` (6.8.1).
c1c = empty @[Int] @Int
