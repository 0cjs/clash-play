#!/usr/bin/env bash
basedir=$(cd "$(dirname "$0")" && pwd -P)
cd "$basedir"   # Set CWD so stack.yaml is used.

#   `stack ghci` will automatically use the options from the the
#   `ghc-options:` section of package.yaml. `stack exec` does not do this,
#   so we need manually to pass them on. We should change this to pull them
#   out of `ghc-options:` rather than duplicating them here.
#
ghc_options='-fwarn-incomplete-patterns'

#   For extensions, as well as {-# LANGUAGE ... -} interpreters use:
#   - `stash exec ghci` uses only what's specified on the command line/in files
#   - `stash ghci` uses extensions listed in `package.yaml`
#   - `clashi` uses an internal set, ignoring `package.yaml`
#   If we need additional extensions not in clashi's internal set, we currently
#   just add `{-# LANGUAGE ... #-}` to the individual files; it would be
#   nice somehow to read these from `package.yaml` and pass them in.

#   We do not need `--package clash-ghc` because stack.yaml dependencies
#   ensure that clash-ghc is in stack's search path.
stack exec -- clashi $ghc_options "$@"
