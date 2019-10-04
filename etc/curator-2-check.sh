#!/usr/bin/env bash

set -euxo pipefail

ETC=$(cd $(dirname $0) ; pwd)
export GHCVER=$(sed -n "s/^ghc-version: \"\(.*\)\"/\1/p" "$ETC/../build-constraints.yaml")

# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# Get new Stackage curator
CURATOR2=stackage-curator-2-f6258124cff9a7e92bcb5704164a70e149080e88
wget "https://download.fpcomplete.com/stackage-curator-2/$CURATOR2.bz2"
bunzip2 "$CURATOR2.bz2"
chmod +x $CURATOR2
mv $CURATOR2 ~/.local/bin/stackage-curator-2

# Install GHC
stack setup $GHCVER

# curator's constraint command has target as a required parameter
# because of a different constraints handling in minor LTS version bumps
NIGHTLY="nightly-$(date +%Y-%m-%d)"
# New curator check
stackage-curator-2 update &&
  stackage-curator-2 constraints --target=$NIGHTLY &&
  stackage-curator-2 snapshot-incomplete &&
  stackage-curator-2 snapshot &&
  stack --resolver ghc-$GHCVER exec stackage-curator-2 check-snapshot
