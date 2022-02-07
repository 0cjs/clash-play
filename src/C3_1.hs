{-# LANGUAGE UnicodeSyntax #-}

module C3_1 where

import Clash.Prelude
import Test.HUnit

topEntity
    ∷ "BTN" ::: Signal System Bit
    → "LED" ::: Signal System Bit
topEntity = fmap complement

----------------------------------------------------------------------

tests ∷ IO Counts
tests = runTestTT $ TestList [test1]

test1 = TestCase (assertEqual "topEntity" expected actual)
    where
        expected = [high, high, low]
        actual   = sampleN 3 $ topEntity $ fromList input
        input    = [low, low, high]
