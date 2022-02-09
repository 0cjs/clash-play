{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE UnicodeSyntax #-}

module C3_1 (htf_thisModulesTests) where

import qualified Clash.Prelude as CP
import Clash.Prelude hiding ((.&.), (.|.))

import qualified Prelude as P
import Test.Framework (
    TestSuite, makeTestSuite, makeUnitTest, makeLoc, assertEqual_,
    ) -- Just so we can see what's actually be using from Test.Framework

--  Nicer names for things from Clash.Prelude that other packages often
--  use. (Given that these aren't nearly as much of an improvement when
--  used in an applicative context, perhaps we should be doing this for
--  Test.Framework instead?)
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
