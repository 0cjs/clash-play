-- XXX until we figure out how to tell the interpreter to use package.yaml
{-# LANGUAGE UnicodeSyntax #-}

{-
    Without an explicit module declaration, Verilog generation
    (e.g., `:verilog` in clashi) will complain:

        No top-level function called 'topEntity' or 'testBench' found,
        nor any function annotated with a 'Synthesize' or 'TestBench'
        annotation. If you want to synthesize a specific binder in
        "src/Hello.hs", use '-main-is=myTopEntity'.
-}
module Hello where

import Clash.Prelude

topEntity ∷ Signal System Bit → Signal System Bit
topEntity = id

main ∷ IO ()
main = print $ sampleN 3 $ topEntity input1
    where
    input1 = fromList [low, low, high]
