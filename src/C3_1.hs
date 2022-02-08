{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE UnicodeSyntax #-}

module C3_1 (htf_thisModulesTests) where

import qualified Clash.Prelude as CP
import Clash.Prelude hiding ((.&.))

import qualified Prelude as P
import Test.Framework

(∧), (∨) ∷ Bits a => a -> a -> a
(∧) = (CP..&.)
(∨) = (CP..|.)

----------------------------------------------------------------------

complementSignal
    ∷ "BTN" ::: Signal System Bit
    → "LED" ::: Signal System Bit
complementSignal = fmap complement

test_complement = assertEqual expected actual
    where
        expected = [low, high, low]
        actual   = sampleN 3 $ complementSignal $ fromList input
        input    = [high, low, high]

----------------------------------------------------------------------

andSignal
    ∷ "BTN_1" ::: Signal System Bit
    → "BTN_2" ::: Signal System Bit
    → "LED"   ::: Signal System Bit
andSignal a b = (∧) <$> a <*> b

test_andSignal = assertEqual expected actual
    where
        a        = [0, 1, 0, 1]
        b        = [0, 0, 1, 1]
        expected = [0, 0, 0, 1]
        actual   = sampleN datalen $ andSignal (fromList a) (fromList b)
        datalen  = P.foldr1 max $ P.map P.length [a, b, expected]
