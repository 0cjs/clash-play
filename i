#!/usr/bin/env bash
basedir=$(cd "$(dirname "$0")" && pwd -P)
cd "$basedir"   # Set CWD so stack.yaml is used.

#   We do not need `--package clash-ghc` because stack.yaml dependencies
#   ensure that clash-ghc is in stack's search path.
stack exec -- clashi "$@"
