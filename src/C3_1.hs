{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE UnicodeSyntax #-}

module C3_1 (htf_thisModulesTests) where

import Clash.Prelude
import Test.Framework

topEntity
    ∷ "BTN" ::: Signal System Bit
    → "LED" ::: Signal System Bit
topEntity = fmap complement

----------------------------------------------------------------------

test_1 = do assertEqual expected actual
    where
        expected = [high, high, low]
        actual   = sampleN 3 $ topEntity $ fromList input
        input    = [high, low, high]
