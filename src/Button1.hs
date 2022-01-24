-- §2.3.2 p.23 PDF 32
{-# LANGUAGE UnicodeSyntax #-}

module Button1 where

import Clash.Prelude
import Clash.Annotations.TH

topEntity ∷ "BTN" ::: Signal System Bit
          → "LED" ::: Signal System Bit
topEntity = id

makeTopEntity 'topEntity
