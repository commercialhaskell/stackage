#!/usr/bin/env bash

set -euxo pipefail

ETC=$(cd $(dirname $0) ; pwd)
export GHCVER=$(sed -n "s/^ghc-version: \"\(.*\)\"/\1/p" "$ETC/../build-constraints.yaml")

# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# Get new Stackage curator
curl -L "https://github.com/commercialhaskell/curator/releases/download/commit-dc6e10c5f2144b36794917b512cff13ac5979ff3/curator.bz2" | bunzip2 > curator
chmod +x curator

# Install GHC
stack setup $GHCVER

# curator's constraint command has target as a required parameter
# because of a different constraints handling in minor LTS version bumps
NIGHTLY="nightly-$(date +%Y-%m-%d)"
# New curator check
./curator update &&
  ./curator constraints --target=$NIGHTLY &&
  ./curator snapshot-incomplete --target=$NIGHTLY &&
  ./curator snapshot &&
  stack --resolver ghc-$GHCVER exec ./curator check-snapshot
