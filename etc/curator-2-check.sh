#!/usr/bin/env bash

set -euxo pipefail

export GHCVER=8.6.3

# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# Get new Stackage curator
CURATOR2=stackage-curator-2-6ec4ac4ee5016e2ecd86af2abfa722b94d2a56c3
wget "https://download.fpcomplete.com/stackage-curator-2/$CURATOR2.bz2"
bunzip2 "$CURATOR2.bz2"
chmod +x $CURATOR2
mv $CURATOR2 ~/.local/bin/stackage-curator-2

# Install GHC
stack setup $GHCVER

# New curator check
stackage-curator-2 update &&
  stackage-curator-2 constraints &&
  stackage-curator-2 snapshotincomplete &&
  stackage-curator-2 snapshot &&
  stack --resolver ghc-$GHCVER exec stackage-curator-2 checksnapshot
