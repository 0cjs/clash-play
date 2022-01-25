東レ Meeting Notes
==================

### 2022-01-19 (Wed)

Meetup notes:
- Stack install: `curl -sSL https://get.haskellstack.org/ | sh`
- [retroclash-book-code]'s `stack install` is missing two dependencies on
  Debian, so: `sudo apt-get install libtinfo-dev libsdl2-dev`.
- The repo uses [clash-shake] to build. See `Shakefile.hs`.
- `stack build` to install Clash and all dependencies. `stack install` will
  copy `bounce-state` and `patterns-sim` to `~/.local/bin`. Both these
  appear to require a graphics display to run.
- In case it's helpful: [Functors, Applicative Functors and Monoids][af]
  from _Learn You a Haskell._

Next time:
- New repo for our code and notes?
- Run all chapter 2 programs in simulator.
- Look at how to hand the output to a vendor toolchain?

Additional research:
- `stack exec --package clash-ghc -- clashi` to run the interpreter.

### Day-by-day

Glasgow 21:00, Tokyo 06:00 next day.

2022-01-19/20 (Wed/Thu)
- `stack exec clashi` in book-code (must have done `stack build` first)
- `stack init` to do minimal stack project setup in current dir; but ended up
  just copying and tweaking the `book-code` file.
- `stack run Hello` to run our program

2022-01-23/24 (Sun/Mon)
- interpreter run script
- Remove clash-play.cabal
- Lots of type discussion and investigation
- Started `src/Active.hs` to work through §2.1 (PDF p.28) example
  (not yet worked through)

2022-01-24/25 (Mon/Tue)
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

2022-01-24/25bis w/Stuart
- Add `module Hello where` to `src/Hello.hs` and now `:verilog` works
  in clashi. Generates files under `verilog/` in the CWD.

### Future Work

- Fix tmate issues with `Esc` timeout and, if we can find it, `C-p`
- Work thorugh details of `src/Button1.hs`, including `makeTopEntity`
  declaration. (Examine generated files under `.stack-work/`?)
- Work through §2.1 example in `src/Active.hs` (and use better names?)
- Make `i` run the interpreter with compiler directives from the
  `default-extensions` list from `package.yaml`.
- Better tests run by `Test`.



<!-------------------------------------------------------------------->
[retroclash-book-code]: https://github.com/gergoerdi/retroclash-book-code.git
[clash-shake]: https://hackage.haskell.org/package/clash-shake
[af]: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
