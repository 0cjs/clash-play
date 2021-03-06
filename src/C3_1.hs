{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE UnicodeSyntax #-}

module C3_1 (htf_thisModulesTests) where

import qualified Clash.Prelude as CP
import Clash.Prelude hiding ((.&.), (.|.))

import qualified Prelude as P
import Test.Framework
    ( TestSuite, makeTestSuite, makeUnitTest, makeQuickCheckTest, makeLoc
    , assertEqual_, qcAssertion
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

{-  Per the example in §6.2.1 p.85 we can do QuickCheck tests directly
    on our pure functions used in synthesis.

    It seems to make sense to do this on Bools, since we can then compare
    our result with the result returned by doing the same operation using
    functions from the standard Haskell Prelude. But we should look at
    how and whether we might do this on the actual Clash types.
-}
prop_andSignal ∷ Bool → Bool → Bool
prop_andSignal a b = expected == actual
    where
        expected = a && b
        actual   = bitToBool $ P.head $ sampleN 1 $ andSignal
            (fromList [boolToBit a]) (fromList [boolToBit b])

----------------------------------------------------------------------

andPairedSignal
    ∷ "BTN" :::
      ( "1" ::: Signal System Bit
      , "2" ::: Signal System Bit
      )
    → "LED"   ::: Signal System Bit
andPairedSignal (a,b) = (∧) <$> a <*> b

test_andPairedSignal = assertEqual expected actual
    where
        a        = [0, 1, 0, 1]
        b        = [0, 0, 1, 1]
        expected = [0, 0, 0, 1]
        actual   = sampleN datalen $ andPairedSignal (fromList a, fromList b)
        datalen  = P.foldr1 max $ P.map P.length [a, b, expected]

----------------------------------------------------------------------

bothEitherSignal
    ∷ "BTN_1" :::
      ( "1" ::: Signal System Bit
      , "2" ::: Signal System Bit
      )
    → "LED" :::
      ( "1" ::: Signal System Bit
      , "2" ::: Signal System Bit
      )
bothEitherSignal (btn1, btn2) = (both, either)
    where
        both    = (∧) <$> btn1 <*> btn2
        either  = (∨) <$> btn1 <*> btn2

test_bothEitherSignal ∷ IO ()
test_bothEitherSignal =
    assertEqual (expectedBoth, expectedEither) (sampledBoth, sampledEither)
    where
        a               = [0, 1, 0, 1]
        b               = [0, 0, 1, 1]
        expectedBoth    = [0, 0, 0, 1]
        expectedEither  = [0, 1, 1, 1]
        (both, either)  = bothEitherSignal (fromList a, fromList b)
        datalen         = P.foldr1 max $ P.map P.length
                            [a, b, expectedBoth, expectedEither]
        sampledBoth     = sampleN datalen $ both
        sampledEither   = sampleN datalen $ either

----------------------------------------------------------------------
-- §3.3 p.30 P.39

revSwitches
    ∷ "SWITCHES" ::: Signal System (Vec 8 Bit)
    → "LEDS"     ::: Signal System (Vec 8 Bit)
revSwitches switches = reverse <$> switches

test_revSwitches ∷ IO ()
test_revSwitches =
    assertEqual expected actual
    where
        input       = 0:>1:>0:>0:>1:>1:>1:>0:>Nil ∷ Vec 8 Bit
        expected    = 0:>1:>1:>1:>0:>0:>1:>0:>Nil ∷ Vec 8 Bit
        actual = P.head $ sampleN 1 $ revSwitches (fromList [input])
        datalen = P.length expected
