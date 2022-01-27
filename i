#!/usr/bin/env bash
basedir=$(cd "$(dirname "$0")" && pwd -P)
cd "$basedir"   # Set CWD so stack.yaml is used.

#   `stack ghci` will automatically use the options from the the
#   `ghc-options:` section of package.yaml. `stack exec` does not do this,
#   so we need manually to pass them on. We should change this to pull them
#   out of `ghc-options:` rather than duplicating them here.
#
ghc_options='-fwarn-incomplete-patterns'

#   We do not need `--package clash-ghc` because stack.yaml dependencies
#   ensure that clash-ghc is in stack's search path.
stack exec -- clashi $ghc_options "$@"
