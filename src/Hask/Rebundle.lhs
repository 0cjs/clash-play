Bundle Implementation Experimentation
=====================================

This [Literate Haskell][lhs] file re-implements the Clash `Bundle`
typeclass so we can experiment with it. It's mainly about Haskell
features such as functional dependencies and associated types.

This is a plain old Haskell module, not connected with Clash in any way,
though it is built within a Clash project, so we already have all the
standard Clash extensions turned on, including NoImplicitPrelude. We turn
on extensions such as GADTs, KindSignatures anyway so that we can load this
with `stack exec ghci` or in any other environment.

> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE GADTs, KindSignatures #-}
> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

> module Hask.Rebundle () where
>
> import Prelude                -- Built in a NoImplicitPrelude environment.
> import Data.Kind (Type)       -- This is also known as `*`

----------------------------------------------------------------------

In our work here, `Signal` doesn't do anything, it's basically just a
placeholder to keep the idea that the class has a dependency on an external
type.

> data Signal τ α = Signal τ α

----------------------------------------------------------------------

XXX Starting here from 2022-02-24 [meeting notes](../../Meet.hs).

The core of the `Bundle` class is:

> class Bundle0 α where
>   type Υ τ α    -- A type function Υ that, given τ and α, produces
>                 -- a new type to be used below.

>   bundle0   ∷ Υ τ α      → Signal τ α
>   unbundle0 ∷ Signal τ α → Υ τ α



<!-------------------------------------------------------------------->
[lhs]: https://wiki.haskell.org/Literate_programming
