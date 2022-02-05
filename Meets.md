東レ Meeting Notes
==================

Future Work
-----------

Short-term:
- Fix tmate issues with `C-p` being held until next char is sent
- HaskExamples datatype promotion section: use `Nat` type variable

Long-term:
- `i` fixes to use standard environment from `package.yaml`:
  - Should use compiler directives from `default-extensions:`
  - Should use options from `ghc-options:`
- Better tests run by `Test`.


Day-by-day
----------

Glasgow 21:00, Tokyo 06:00 next day. Weekdays are m/t/w/r/f/s/u.

2022-02-04/05 (s/u)
- HaskExamples: ADTs vs. GADTs
  - The two are always distinguished syntatically with `=` vs. `where`
    after the `data ...`.
  - GADT declarations infer Kind only; unlike ADTs the variable names are
    ignored.
- IsActive `data Polarity = High | Low`:
  - These data constructors are never used; they exist only to create the
    promoted type constructors created by the `DataKinds` extension.
  - Were this a library, the interface is just `Active High` and `Active
    Low`  (e.g., `Signal System (Active Low)`); all the rest is just
    infrastructure.
- §2.3.3 (p.24) exercise:
  - You can swap the `Signal System Bit` for `Signal System Bool` in
    `topEntity` with no problems, so long as you change the input you give
    it from `[0, 0, 1]` to `[False, False, True]`.
  - Convention is to use `Bit` for levels (0=GND 1=Vcc) and `Bool` for
    signal values (e.g., `/RESET` asserted = True = 0 = low level).
- cjs/sjn discuss automated testing. Roughly it looks like this:
  - Virtually all of the design testing is run in the Clash simulator
    driven by Haskell code. That driver generates test vectors and expected
    output (either hard coded or programatically generated), feeds the
    input to the simulator and compares the simulator's output with the
    expected output. (This can be done both for units of the code and for
    the fully synthesized design, à la regular software unit and functional
    tests.)
  - The remainder of the automated testing is, "the vendor HDL compiler
    successfully compiles the code." This will catch things like "this chip
    can't meet the timing constraints (i.e., run at the speed) you
    requested."
  - Further non-automated debugging can be done with:
    - The vendor's HDL toolchain, which can e.g. add a logic analyzer into
      the design, let us feed its simulator stuff and watch the output.
    - 'Scope on board.
    - If the above finds problems in our Clash design, we generate new
      Clash simulator tests to get coverage of that. If it finds problems
      on our board, we fix the PCB. It should never find problems with the
      FPGA itself or the vendor HDL toolchain (except perhaps in how our
      build system feeds the vendor toolchain).
  - Next step in our testing framework is to expand `Hello` to check the
    output? Alternatively, we can go with an input file fed to `main` which
    prints output and which we then externally compare with a diff program.

2022-01-28/29 (f/s) cjs,sjn,croys
- Discussion of parameters to type constructors and data constructors

2022-01-27/28 (r/f) cjs,sjn,croys
- Partial fix for GHC options for `i`
- GADTs worked out (`src/HaskStuff.hs`)
- Use of extensions in interpreters:
  - `stash exec ghci` uses only what's specified on the command line/in files
  - `stash ghci` uses extensions listed in `package.yaml`
  - `clashi` uses an internal set, ignoring `package.yaml`
  - An extension you need for both compilation and clashi interpreter that is
    not already in the standard Clash set must be added as a `{-# LANGUAGE ...
    #-}` directive (until we find something better; maybe a hack in `i`?) Some
    extensions, such as NoStarIsType may simply break the Clash interpreter (it
    produces a mysterious error).
- Add "Implicit kinds and KindSignatures" to `src/HaskExamples.hs` showing
  implicit inference of kinds (which Haskell has been doing for type classes
  all along) and explicit signature equivalance.
- Clash uses `NoStarIsType`; see [8.6.x migration] for that vs. `StarIsType`.
  (That example assumes the [TypeOperators] extension.)
- HaskExamples datatype promotion section currently covers use of `Type` (i.e.
  `*`) only; we still need to extend the example to use a more constrained kind
  variable of `Type → Nat → Type` instead of `Type → Type → Type`.

[8.6.x migration]: https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/8.6
[TypeOperators]: https://typeclasses.com/ghc/type-operators

2022-01-26/27 (w/r) cjs,sjn
- Generated files under `.stack-work/` do not include the TH-processed source.
  So we'll just look further at what exactly the TH stuff is doing later.
- Naming in `Active.hs` is a bit confusing
- Working through `Active.hs` we realized:
  - In `instance isActive High`, `High` is not a type or type constructor, it's
    a _data constructor_ (i.e., in the data language, not the type langauge, as
    cjs thinks of things).
  - It turns out that we're here using _datatype promotion_ [[GHC §7.9]] from
    the `-XDataKinds` extension. (cjs needs to read up again on kinds as a
    refresher.)
  - Looking at that cjs was reminded of GADTs [[GHC §7.4.8]] (`-XGADTs`), and
    we started looking at that. cjs needs a refresher on the motivation for
    that extension, so the next exercise will be to walk through the example
    given at the start of that section and demonstrate the problem that occurs
    if you try to do it without GADTs. (Then maybe read through the previous
    section "Declaring data types with explicit constructor signatures" [[GHC
    §7.4.7]] (`-XGADTSyntax`) as well; there must be something important in it
    if it's that long.)

[GHC §7.9]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/promotion.html
[GHC §7.4.8]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/data-type-extensions.html#gadt
[GHC §7.4.7]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/data-type-extensions.html#gadt-style

2022-01-25/26 post-meet research by cjs:
- I've [documented in sedoc][sedoc/main] the GHC [`-main-is ⟨thing⟩`]
  option, Cabal `main-is:` (which is an entirely different thing!) and the
  `main:` options (a combination of the two above) for hpack/Stack
  `package.yaml`files.

[`-main-is ⟨thing⟩`]: https://downloads.haskell.org/~ghc/9.2.1-rc1/docs/html/users_guide/phases.html#ghc-flag--main-is%20%E2%9F%A8thing%E2%9F%A9
[sedoc/main]: https://github.com/0cjs/sedoc/blob/master/lang/haskell/main.md

2022-01-25/26 (t/w) cjs,sjn
- `tmate show-options -s | grep esc` to see that default esc timeout is 500
  (in milliseconds); `tmate set-option -g escape-time 0` to remove the
  delay. cjs is documenting more of the messy tmate interface in sedoc
  later.
- `package.yaml` is part of the [hpack] standard, a modern alternative to
  Cabal. cabal2nix and stack natively support this; an `hpack` executable
  can generate a `.cabal` file for other systems.
- `src/HaskExamples.hs` was failing becuse `Show` wasn't defined; our build
  definition was using `NoImplicitPrelude` so we needed to add an explicit
  `import Prelude`. (This was not a problem when we were running the
  interpreter stand-alone because those `project.yaml` directives are not
  used by stack when running the interpreter, only the compiler.)
- The [GHC docs §5.9.2][ghc5.9.2] state that the entry point must be
  Main.min. This was definitely not true 15 years ago, and appears still to
  be untrue. In the cabal `main-is:` argument if you specify a string
  ending in `.hs` it takes this as a file that must contain `Main.main`.
  However, if you specify a module name and definition (which must be an
  `IO ()` action) it will happily use that, e.g., `Hello.myEntryPoint`.
- After somne thought, we've decided not to commit the `clash-play.cabal`
  generated from `package.yaml`. Whether it's better to commit or not
  depends on the situation; at the moment not commiting seems easier
  for us. For discussion about the advantages and problems from committing
  the `.cabal` files see [stack #5210] and [snoyman 2020-03-04]. Also
  [stack.yaml vs cabal package file][package-yaml] provides some insight
  into what `package.yaml` is.

[ghc5.9.2]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/packages.html#the-main-package
[hpack]: https://github.com/sol/hpack
[package-yaml]: https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/
[snoyman 2020-03-04]: https://www.fpcomplete.com/blog/storing-generated-cabal-files/
[stack #5210]: https://github.com/commercialhaskell/stack/issues/5210

2022-01-24/25bis w/Stuart
- Add `module Hello where` to `src/Hello.hs` and now `:verilog` works
  in clashi. Generates files under `verilog/` in the CWD.

2022-01-24/25 (m/t)
- src/Button1.hs from §2.3.2 p.23 PDF 32
- work out mappings to hardware in `book-code/book-code/src/led/button/`:
  - `src/Button.hs` uses TH (Template Haskell) to generate `BTN_1`, `LED_2`
    etc. names
    - The `topEntity ∷ "BTN" ::: { "1" :: Signal System Bit, ...` syntax
      appears to generate a `BTN_1` name that's connected "by hand" in the
      Verilog below.
  - `target/nexys-a7-50t/src-hdl/Top.v` is a Verilog file that declares the
    Verilog module (`Top`) for that particular board, along with the inputs
    and outputs (`BTNL`/`LED[1]`/etc.) being used, and then connects those
    names to the `topEntity`/`BTN_1`/`LED_2`/etc. names from `Button.hs`
  - `target/nexys-a7-50t/src-hdl/nexys-a7-50t.xdc` is a Xilinx Design
    Constraints file (though generic to many brands these days) containing
    TCL code (only a limited set of functions is accepted) that:
    - connect the port names `BTNL`/`LED[1]`/... to specific pins in the
      package. (There may be FPGA-specific constraints on what pins can be
      used for various functions; you just need to get these right or
      you'll get an error later in the build, perhaps hours later.)
    - Set voltage and other parameters for the pins
    - Specify which pin is the clock input and what the clock rate is, and
      optionally further information for use in the timing constraints
      calculations.
    - Some information is not checked between `.v` and `.xdc`, such as
      whether a GPIO is input or output. It's the developer's
      responsibility to avoid, e.g., using a pin wired to a switch as an
      output set to high that gets shorted when the switch is closed.

2022-01-23/24 (u/m)
- interpreter run script
- Remove clash-play.cabal
- Lots of type discussion and investigation
- Started `src/Active.hs` to work through §2.1 (PDF p.28) example
  (not yet worked through)

2022-01-19/20 (w/r)
- `stack exec clashi` in book-code (must have done `stack build` first)
- `stack init` to do minimal stack project setup in current dir; but ended up
  just copying and tweaking the `book-code` file.
- `stack run Hello` to run our program

2022-01-18/19 (w/r)
- Meetup notes:
  - Stack install: `curl -sSL https://get.haskellstack.org/ | sh`
  - [retroclash-book-code]'s `stack install` is missing two dependencies on
    Debian, so: `sudo apt-get install libtinfo-dev libsdl2-dev`.
  - The repo uses [clash-shake] to build. See `Shakefile.hs`.
  - `stack build` to install Clash and all dependencies. `stack install` will
    copy `bounce-state` and `patterns-sim` to `~/.local/bin`. Both these
    appear to require a graphics display to run.
  - In case it's helpful: [Functors, Applicative Functors and Monoids][af]
    from _Learn You a Haskell._
- Next time:
  - New repo for our code and notes?
  - Run all chapter 2 programs in simulator.
  - Look at how to hand the output to a vendor toolchain?
- Additional research:
  - `stack exec --package clash-ghc -- clashi` to run the interpreter.

[af]: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
[retroclash-book-code]: https://github.com/gergoerdi/retroclash-book-code.git
[clash-shake]: https://hackage.haskell.org/package/clash-shake
