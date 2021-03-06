-- Simple Haskell syntax example and play
{-# LANGUAGE UnicodeSyntax #-}
-- The following allow us to load this in `stack exec ghci`:
{-# LANGUAGE GADTs, KindSignatures #-}

module Hask.Examples where

import Prelude
import Data.Kind (Type)             -- also Clash.Prelude (Type)

----------------------------------------------------------------------
-- Infix data constructors

data List1 α = Nil1 | Cons α (List1 α)      deriving (Show, Eq, Ord)

-- Infix data constructor :> must start with a `:`, not any other punctuation.
data List2 α = Nil2 | α :> (List2 α)        deriving (Show, Eq, Ord)
infixr 8 :>

-- Infix variable >+ must not start with a `:`.
-- (Remember, even "functions" are just variables bound to a lambda.)
(>+)   ∷ Bool → Bool → Bool
_ >+ _ = False

----------------------------------------------------------------------
-- GADTs

-- GADTs §7.4.8 Based on example from
-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/data-type-extensions.html#gadt

data Term α where
    I ∷ Int → Term Int
    B ∷ Bool → Term Bool

eval ∷ Term α → α
eval (I x) = x      -- ∷ Term Int  → Int
eval (B x) = x      -- ∷ Term Bool → Bool

{- This doesn't work because the following patterns are incomplete:

succ ∷ Term → Term
succ (I x) = I (x + 1)

isZero ∷ Term → Term
isZero (I 0) = B True
isZero (I _) = B False
-}

----------------------------------------------------------------------
--   Implicit kinds and KindSignatures
--
--   Note that Clash uses `NoStarIsType`, so when using Clash you msut
--   specify e.g. `Type → Type` instead of `* → *`

--   `:k UnKindly` in the interpreter gives `UnKindly :: Type -> Constraint`
--   because the output cannot be any type, but must be only a type that's
--   a member of class UnKindly.
class UnKindly α where      -- ∷ Type → Constraint
    ukf ∷ α → Bool

--   The application of α to another type in type definition `kf` makes the
--   type checker to infer α's kind as (* → *), which works because we have
--   no explicit kind signature for Kindly.
class Kindly α where        -- ∷ (* → *) → Constraint
    kf ∷ α Int → Bool

--   With KindSignatures we can explicitly specify the type of arguments
--   to type constructors (using `Type` because `NoStarIsType` enabled).
class ExplicitKindly (α ∷ Type → Type) where
    ekf ∷ α Int → Bool

----------------------------------------------------------------------
-- Datatype Promotion (DataKinds extension)
-- https://downloads.haskell.org/~ghc/8.10.4/docs/html/users_guide/glasgow_exts.html#datatype-promotion
--
-- `Type` below is from the Clash prelude which import/exports Data.Kind.Type
-- Older docs §7.9.1; breaks due to NoStarIsType (see 2nd URL below)
-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/promotion.html
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/8.6

--  Peano numerals as type constructors only to parametrize a type
data Ze     -- Zero length
data Su n   -- Successor (Su Ze = 1, etc.)
            -- Problem is, you can say `Su Char` because _n_ not restricted

--  KindSignatures extension gives us the `∷ Type → ...` ability
data Vec ∷ Type → Type → Type where
    VNil  ∷ Vec a Ze
    VCons ∷ a → Vec a n → Vec a (Su n)

--  If we do not provide an explicit kind signature we must provide the
--  parameters after the type name that it infers the correct
--  kind * → * → * signature rather than just kind *.
data VecInf a n where
    VINil  ∷ VecInf a Ze
    VICons ∷ a → VecInf a n → VecInf a (Su n)

----------------------------------------------------------------------
-- ADTs vs. GADTs

-- Renaming a parameter does not change the type of a function:
f, g ∷ Int -> Int
f x = x             -- f y = y would be the _same_ function as f x = x.
                    -- This is "alpha equivalence."
g y = y             -- Same type.

-- ADT (because using `data … =` syntax)
data A α β          -- Introduces bound variables, but cannot constrain
                    --    them here to same type by using same name.
    = A1 α α        -- Constrains types to be same only in this constructor.
 -- | A2 γ γ        -- Not allowed because γ is free.

-- GADT (because using `data … where` syntax)
data P1 α β where       -- Variable _names_ are meaningless; just kind inferred.
    Q1 ∷ γ → δ → P1 γ δ -- is the same type as Q ∷ α → β → P α β

-- Thus more clear to use KindSignatures with GADTs:
data P2 ∷ Type → Type → Type where
    Q2 ∷ γ → δ → P2 γ δ
