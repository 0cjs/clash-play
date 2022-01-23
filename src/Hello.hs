-- XXX until we figure out how to tell the interpreter to use package.yaml
{-# LANGUAGE UnicodeSyntax #-}

import Clash.Prelude

topEntity ∷ Signal System Bit → Signal System Bit
topEntity = id

main ∷ IO ()
main = print $ sampleN 3 $ topEntity input1
    where
    input1 = fromList [low, low, high]
