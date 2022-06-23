東レ Meeting Notes
==================

Future Work
-----------

Short-term:
- 2022-02-07/08 to-do
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

#### 2022-02-25 (f) cjs sjn

- Moved `HaskExamples` to `Hask.Examples`; we're starting a new subdir for
  modules that are not Clash stuff but just about learning Haskell itself.
  Our new code for stuff below is in `Hask.FDs`. (The `hello` executable
  builds everything under `src/` so we don't need to worry about adding
  anything more to have these "tested" by the compiler.)
- There's a good explanation/tutorial in GHC manual §[6.8.7.2].
- Read `| a b → c` as "_a_ and _b_ determine a unique c"; i.e., for any
  given _a_ and _b_ there may be only one _c._
- Sample code in `src/HaskExamples.hs`.
- Maybe since `TypeApplications` [6.4.14] introduced in v8.0.1 FDs (since
  v6.8.1) no longer let us write code that we couldn't write before.
- But we still have to turn on `AllowAmbiguousTypes` [6.11.2] (v7.8.1) to
  defer the checking to call sites, otherwise it points out that there's
  ambiguity in the class definition.
- [so 4950] gives a good explanation of what's going on with the ambiguity.
  In short, given a constraint `Foo a => ...` (implicit in instance
  definitions), we are telling the compiler that it must check that `a` is
  a member of class `Foo` for any use of that function, and if no value of
  type `a` appears, there's no way to do that check. With
  `AllowAmbiguousTypes` off, the compiler can tell us at definition time
  that it's impossible (without `TypeApplications`) to write a call to that
  function that isn't ambiguous.
  - From a comment: "An ambiguous type is one which has a type variable in
    its context which is not mentioned in the body (that to the right of
    =>) of the type. So RealFloat a => .. is ambiguous when .. doesn't
    mention a. Ambiguous types are generally a programmer error, and before
    TypeApplications they were entirely useless...."
- Stopping now; cjs will read up on this stuff further.

[6.8.7]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/functional_dependencies.html
[6.8.7.2]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/functional_dependencies.html#background-on-functional-dependencies
[6.4.14]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_applications.html
[6.11.2]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/ambiguous_types.html
[so 4950]: https://stackoverflow.com/a/45664950/107294

#### 2022-02-24 (r) cjs sjn

Looking again at syntax for [12.9.4.1 Associated Instances][12.9.4.1] and
[12.9.4.2 Associated type synonym defaults][12.9.4.2].

Below we use the following variables.
- `τ` (_tau_): the clock domain parameter for `Signal`s. Remember via the
  Greek word for "domain" τομέα or the physics τ for "time."
- `α` (_alpha_): The type of the bundle holding the thing or things we can
  unbundle.
- `Υ` (Upsilon): a type constructor taking two arguments. This is
  upper-case to distinguish it from type variables which take no arguments.
  This function, when applied to its parameters, gives us the unbundled
  type.
- `Vec ι ε` (vector of _iota_ length having elements of type _epsilon_).
  - ι as in the APL ι, [C++ iota()] or [Go iota]: "The _integer_ function
    denoted by ι produces a vector of the first _N_ integers."
  - ε being the source of set notation's ∈, "is an element of."

The core of the `Bundle` class is:

    class Bundle α where
      type Υ τ α    -- A type function Υ that, given τ and α, produces
                    -- a new type to be used below.

      bundle    ∷ Υ τ α      → Signal τ α
      unbundle  ∷ Signal τ α → Υ τ α

      -- Actually, the real associated type declaration looks as if it adds
      -- a functional dependency (on a separate line); we elide this below
      -- but see the end for more.
      type Υ (τ ∷ Domain) α = res | res -> τ α

However, the actual implementation (in `Clash.Signal.Bundle`) includes some
default definitions. For whatever reason, we explicitly say `default` for
the function default types and instantiations, but not for the default
instantiation of the `Υ τ α` type.

    class Bundle α where
      type Υ τ α = Signal τ α   -- default implementation of type function Υ

      bundle    ∷ Υ τ α      → Signal τ α
      unbundle  ∷ Signal τ α → Υ τ α

      default   bundle ∷ (Signal τ α ~ Υ τ α) => Υ τ α      → Signal τ α
      bundle s = s

      default unbundle ∷ (Υ τ α ~ Signal τ α) => Signal τ α → Υ τ α
      unbundle s = s

This allows us to easily use the default implementation for things where
the "unbundled" version is the same as the bundled version, e.g.:

    instance Bundle ()
    instance Bundle Bool
    instance Bundle Int
    instance Bundle (Maybe α)

When the unbundled and bundled types are different we must explicitly
define the unbundled type (implicitly defining the function types as well,
if we don't explicitly give them) and the function bodies. Here we must
subtitute `Vec ι ε` for α as used in `Υ τ α`, and on the right hand side.

    instance KnownNat n => Bundle (Vec ι ε) where

      -- More complex implementation of Υ:
      -- 1. We pattern match to deconstruct the second parameter.
      -- 2. Our result type is not so exactly parallel to the input type.
      -- But still applying it works the same way: anywhere we see `Υ τ α`
      -- we first substitute α and then substitute the RHS for the LHS.

      type Υ τ (Vec ι ε) = Vec ι (Signal τ ε)

      --       ∷ Υ τ α              → Signal τ α         -- class definition
      --       ∷ Υ τ (Vec ι ε)      → Signal τ (Vec ι ε) -- subst. α definition
      bundle   ∷ Vec ι (Signal τ ε) → Signal τ (Vec ι ε) -- subst. Υ definition
      bundle   = traverse# id

      -- unbundle is same as above, with arg and result swapped
      unbundle = sequenceA . fmap lazyV

The one bit we didn't cover is the actual declaration of the associated
type synonym:

    type Unbundled (dom :: Domain) a = res | res -> dom a

We think that this is a functional dependency, but need to go look up how
those work to understand it.

Todo next session:
- Review the above, make sure it's correct, and that we still understand it.
- Investigate the [functional dependency][6.8.7] `res | res -> dom a`.

[C++ iota()]: https://sean-parent.stlab.cc/2019/01/04/iota.html
[Go iota]: https://go.dev/ref/spec#Iota
[12.9.4.1]: https://downloads.haskell.org/~ghc/8.10.4/docs/html/users_guide/glasgow_exts.html#associated-instances
[12.9.4.2]: https://downloads.haskell.org/~ghc/8.10.4/docs/html/users_guide/glasgow_exts.html#associated-type-synonym-defaults
[6.8.7]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/functional_dependencies.html

#### 2022-02-22 (t) cjs sjn croys

- Had a look at some QuickCheck stuff and better understood it.
- p.31/P.40: Worked out how the `unbundleVec` function works:
  - `unbundleVec bus = map (\i -> (!!i) <$> bus) indices`
- Looked at the `instance Bundle (Vec n a)` to understand `class Bundle`.
  Insight: type families extend classes to let you have not just new
  functions defined for each instance, but also new types defined for each
  instance. In this case, we need to define what a bundle is and what its
  unbundled version is, though we don't quite see how the versions in
  `unbundleVec` on p.33 (bundled: `Signal dom (Vec n a)`;
  unbundled: `Vec n (Signal dom a0`) match up with the instance
  declaration: `type Unbundled t (Vec n a) = Vec n (Signal t a)`

2022-02-21 (m) cjs sjn croys
- Did a version of andSignal taking a pair instead of two parameters, and
  that worked easily enough just by passing `(fromList a, fromList b)` as
  the argument. So that's not where we were screwing up yesterday.
- Pattern matched the two signals to separate variables and sampled them
  separately. sjn thinks that as we move on we'll not be using Bit outputs,
  and so we don't need to worry about zipping and unzipping sets of bits
  for more convenient reading.
- Typed in example from §3.3 p.30 and after much mucking about with types
  got a basic test working for it.

2022-02-17/18 (t/f) cjs sjn
- Tried the next example on page 28 (we're calling it `bothEitherSignal`)
  and couldn't get the types to match up in the unit test. The problem is
  that we're not dealing properly with pairs of signals going in and out.
- TODO: We should split this into two parts: first a pair of signals going
  in and a single signal coming out, and get that working, then a pair of
  signals coming out.

2022-02-15/16 (w/r) cjs
- `prop_andSignal` was fixed by Stuart using `boolToBit` and `bitToBool`. A
  property failure output is none too clear: changing the property from `a
  && b` to `a` produces the error `Replay argument: "Just (SMGen
  8136674713069414597 3311635235059918567,1)"` Also, running the test
  directly in the interpreter produces "No instance for (Show (Bool -> Bool
  -> Bool))".

2022-02-07/08 (t/w) cjs sjn
- Tests have very slow turnaround: one change to `C3_1` takes 7.5 seconds
  to rebuild and unit test. (Most of the time is spent building.) The
  interpreter reloads very quickly with `:r`, and we can run a test in it
  just by typing its name, but the output is terrible if it fails and
  running `htf_C3_1_thisModulesTests` produces a "no instance for (Show
  Testsuite)" error.
- Want to test testing second example in §3.1 p.28 (P.37) (two inputs, not
  tuple input). §6.2.1 p.85 (P.94) mentions that these functions can be
  tested directly.
- Leave off the above until we get the "two arguments to `topEntity` (which
  we have renamed to `andSignal` to avoid conflict with the earlier example
  in this file). For the moment, just use `sampleN` as before.
- `Clash.Prelude` and `Test.Framework` both define their own `(.&.)`. We
  resolve this for the moment by (slightly awkwardly) renaming the Clash
  version to `(∧)`, which kinda seems more intuitive anyway, though it soon
  doesn't look as intuitive when it's buried in an applicative: `(∧) <$> a
  <*> b`.
- Trying to write the test for that, I'm heading towards something that
  looks like a truth table (yay!) but there are lots of issues with
  overlap/conflicts between the Clash versions and Prelude versions of
  functions such as `length`, `map` and `foldr1`. We `import qualified
  Prelude as P` to get the prelude ones we want for the tests.
- This is all really starting to push towards having the tests in separate
  modules from the code under test in order to avoid having to qualify all
  our names to avoid collisions. But maybe it wouldn't be so bad if we
  factored out some higher level test functions (such as, "confirm this
  truth table") into our own test library. We'll have to see as we move
  forward.
- After all the above, got the new test working and automatically figuring
  out the length of the inputs and outputs (and failing in some way if the
  lengths are not all equal).
- Todo:
  - Can the `(∧) <$> a <*> b` be made prettier, like `a ∧ b`?
  - Faster test turnaround using reload in interpreter. (Problems to deal
    with noted above.)
  - Get QuickCheck version of test working.
  - Figure out how to test without `sampleN` and compare this to testing
    with it.
  - Extract common test code (or not, if due to above we feel we never need
    `sampleN` tests again).

2022-02-06/07 (m/t) cjs sjn
- Type in sample program from §3.1, add unit test
- Switch from plain HUnit to [HTF]
  - The clash resolver provides a version of HTF. HTF has HUnit as a
    dependency, which means that it's always been available through the
    clash resolver but perhaps hidden in some way? At any rate, once I add
    `HTF` to `dependencies:` in `package.yaml` I no longer need to specify
    a version of HUnit in `stack.yaml`.
  - Used [TLUG website] as a reference for configuration and setup.
  - New `src/UnitTests.hs` as our master unit test collector and driver. We
    use `unitTestMain` as the main function here to avoid conflicting with
    `Hello.main`; for more on this see below.
  - `src/{Hello,C3_1}.hs` updated to have HTF tests.
  - In `package.yaml` for the `hello` executable we get a warning that
    `Hello` should be added to `exposed-modules` or `other-modules` in the
    `.cabal` file.
    - This is kind of fixed by adding `other-modules: Hello`, as it
      suggests, though that in turn emits another (smaller) warning:
      `Warning: Enabling workaround for Main module 'Hello' listed in
      'other-modules'`. There's also an `illegally!` that pops out alone
      on a line near there; it's not clear what that's connected to.
    - It's not clear what's going on with this and it should probably be
      investigated at some point, though possibly this will be fixed by
      reworking how we deal with `main` functions (see below).
  - Document in [README](../README.md) how to set up new files and tests.
- The `main` conflicts above and other issues related to `main` are
  probably best fixed by removing all `main` functions from files under
  `src/` so that they're not automatically all compiled together by the
  `source-dirs: src/` parameter. (Just giving each main function under
  `src/` a different name doesn't help.) For the [TLUG website] project we
  have a separate subdirectory called `app/` containing a file for the
  `module Main` of each stand-alone program.

[HTF]: https://hackage.haskell.org/package/HTF
[TLUG website]: https://github.com/tlug/tlug.jp.git

2022-02-05/06 (s/m)
- HUnit test for Hello.hs
- Runs the simulator just like `main`, with the same input list, and tests that
  the output list is as expected (equal to input since `topEntity = id`).
- We needed to add `HUnit-1.6.2.0` (latest version according to Hackage's
  [Test.HUnit] page) to the `extra-deps:` section of `stack.yaml` because HUnit
  is not included in the `clash-1.4.3` resolver. This does not add it as a
  dependency to any part of our project, it just makes it available from Stack.
- `HUnit` is then added to the `dependencies:` section of `package.yaml` so
  that our code is linked to it.
- A new `tests:` top-level section is added to `package.yaml` to specify a
  program to compile and run that will run the tests. We specify a different
  function (`mainTest`) in the same `Hello` module for this, though of course
  the executable we generate for this has to have a different name
  (`hello-test` instead of `hello`). Eventually this will no doubt be split out
  into a separate module that brings together the tests from all our other
  modules.
- In the `Test` script we change `stack build` to `stack build --test` so all
  test programs are run as well, and we can now drop the `stack run hello`.
- We use just HUnit right now, but it's anticipated we'll add QuickCheck and/or
  SmallCheck (exhaustive testing) later. The book §6.2.1 p.85 gives an example
  of using QuickCheck.
- Though we're running the simulator here, unit tests can also be done on the
  individual pure functions describing the device. (There's no point doing that
  yet becuase the function describing our Hello device is `id`.)

[Test.HUnit]: https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit.html

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
