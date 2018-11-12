#!/usr/bin/env bash

set -euxo pipefail

export GHCVER=8.6.2

# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# Get stackage-curator
wget https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
bunzip2 stackage-curator.bz2
chmod +x stackage-curator
mv stackage-curator ~/.local/bin

# Install GHC
stack setup $GHCVER

# Update the index
stack update

# Check
exec stack --resolver ghc-$GHCVER exec stackage-curator check
