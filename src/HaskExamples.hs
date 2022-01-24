-- Simple Haskell syntax example and play
{-# LANGUAGE UnicodeSyntax #-}

module HaskExamples where

data List1 α = Nil1 | Cons α (List1 α)      deriving (Show, Eq, Ord)

-- Infix data constructor :> must start with a `:`, not any other punctuation.
data List2 α = Nil2 | α :> (List2 α)        deriving (Show, Eq, Ord)
infixr 8 :>

-- Infix variable >+ must not start with a `:`.
-- (Remember, even "functions" are just variables bound to a lambda.)
(>+)   ∷ Bool → Bool → Bool
_ >+ _ = False
