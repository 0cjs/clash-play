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


<!-------------------------------------------------------------------->
[lhs]: https://wiki.haskell.org/Literate_programming
