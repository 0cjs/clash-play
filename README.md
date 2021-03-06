Clash and Retroclash
====================

This repo is just a playground for experimenting with the functional
hardware description language [Clash] and the programs and ideas in the
book [_Retrocomputing with Clash: Haskell for FPGA Hardware Design_][retro].
Some programs here are versions of the example programs in the book, also
available in the [`gergoerdi/retroclash-book-code`][book-code] repo on
GitHub.

Do not expect the code in this repo to work or do anything useful.

Handy notes:
- Further documentation is in various files under [`doc/`](./doc/).
- `Test` builds and (soon) runs a few tests.
- `i` runs the interpreter; pass in filenames to load on the command line
- `reference/` contains clones of various repos whose code one might want
  to rexamine. (These are cloned by `Test`.)

### Adding New Files and Tests

To add a new file with unit tests you can follow the template below. You
can also look at `src/C3_1.hs` as an example. The the documentation for
[HTF], and the [HTF tutorial] may also be useful.

    {-# OPTIONS_GHC -F -pgmF htfpp #-}

    module My Module (htf_thisModulesTests) where

    import Clash.Prelude
    import Test.Framework

    test_mytest = assertEqual "expected" "actual"

- The GHC option [`-F`] indicates that a preprocessor should be run over
  the code just before the compiler proper runs on it (i.e., after the
  literate markup has been stripped away and the C preprocessor has be run
  on it). The [`-pgmF <cmd>`] option gives the HTF preprocessor name.
- The `htf_thisModulesTests` definition will be generated by the
  preprocessor; this is imported by the module that runs all the unit
  tests.
- `Clash.Prelude` must be imported because we use `NoImplicitPrelude` to
  avoid importing the standard prelude.
- `Test.Framework` provides `assertEqual` etc.
- All definitions with prefix `test_` are considered HUnit tests by the HTF
  preprocessor; all definitions with prefix `prop_` are considered
  QuickCheck properties. These will be instrumented so that failures can
  show filename, line number, and expected and actual results.

### Building Verilog/VDHL

- The `:verilog` command in `clashi` will generate Verilog output under a
  `verilog/` directory.
- See the `2022-01-24/25` notes in [Meets] for more information on what's
  generated.
- The [clash-shake] package provides Shake rules for building Clash
  programs and synthesizing FPGA configuration for several different vendor
  toolchains. The GitHub [clash-pong] project provides an example of its
  use.


<!-------------------------------------------------------------------->
[Meets]: ./Meets.md

[book-code]: https://github.com/gergoerdi/retroclash-book-code
[clash-pong]: https://github.com/gergoerdi/clash-pong/
[clash-shake]: https://hackage.haskell.org/package/clash-shake
[clash]: https://clash-lang.org/
[retro]: https://unsafeperform.io/retroclash/

[HTF]: https://hackage.haskell.org/package/HTF
[HTF tutorial]: https://hackage.haskell.org/package/HTF-0.14.0.6/docs/Test-Framework-Tutorial.html
[`-F`]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#ghc-flag--F
[`-pgmF <cmd>`]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#ghc-flag--pgmF%20%E2%9F%A8cmd%E2%9F%A9
