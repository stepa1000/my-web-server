#!/bin/sh

set -eu

cd "$(git rev-parse --show-toplevel)"
# find . -name '*.hs' -print0 | xargs -0n100 ormolu --mode inplace
stack test
stack build
hlint .
