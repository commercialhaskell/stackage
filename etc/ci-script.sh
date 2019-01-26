#!/usr/bin/env bash

set -euxo pipefail

export GHCVER=8.6.3

# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# Get stackage-curator
wget https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
bunzip2 stackage-curator.bz2
chmod +x stackage-curator
mv stackage-curator ~/.local/bin

# Get new stackage-curator
CURATOR2=stackage-curator-2-9132b4b184f1f7f27c4c80ec0609460cea22ae75
wget  "https://s3.amazonaws.com/www.snoyman.com/stackage-curator-2/$CURATOR2.bz2"
bunzip2 "$CURATOR2.bz2"
chmod +x $CURATOR2
mv $CURATOR2 ~/.local/bin/stackage-curator-2

# Install GHC
stack setup $GHCVER

# Update the index
stack update

# Check
stack --resolver ghc-$GHCVER exec stackage-curator check

# New curator check
stackage-curator-2 update &&
  stackage-curator-2 constraints &&
  stackage-curator-2 snapshotincomplete &&
  stackage-curator-2 snapshot &&
  stackage-curator-2 checksnapshot
