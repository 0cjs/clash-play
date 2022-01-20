東レ Meeting Notes
==================

### 2022-01-19

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

### 2022-01-20

Meetup notes:
- `stack exec clashi` in book-code (must have done `stack build` first)
- `stack init` to do minimal stack project setup in current dir; but ended up
  just copying and tweaking the `book-code` file.
- `stack run Hello` to run our program



<!-------------------------------------------------------------------->
[retroclash-book-code]: https://github.com/gergoerdi/retroclash-book-code.git
[clash-shake]: https://hackage.haskell.org/package/clash-shake
[af]: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
