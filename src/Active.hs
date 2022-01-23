-- §2.1 p.19 (PDF 28)
{-# LANGUAGE UnicodeSyntax #-}

module Active where
import Clash.Prelude

data Polarity = High | Low
    deriving (Show, Eq)

newtype Active (p ∷ Polarity) = MkActive { activeLevel ∷ Bit }
    deriving (Show, Eq, Ord, Generic, NFDataX, BitPack)

active ∷ Bit → Active p
active = MkActive

class IsActive p where
    fromActive :: Active p -> Bool
    toActive :: Bool -> Active p

instance IsActive High where
    fromActive = bitToBool . activeLevel
    toActive = MkActive . boolToBit

instance IsActive Low where
    fromActive = bitToBool . complement . activeLevel
    toActive = MkActive . complement . boolToBit
