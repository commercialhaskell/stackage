#!/usr/bin/env bash

set -euxo pipefail

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

# Get new Stackage curator
CURATOR2=stackage-curator-2-6ec4ac4ee5016e2ecd86af2abfa722b94d2a56c3
wget "https://download.fpcomplete.com/stackage-curator-2/$CURATOR2.bz2"
bunzip2 "$CURATOR2.bz2"
chmod +x $CURATOR2
mv $CURATOR2 ~/.local/bin/stackage-curator-2

# New curator check
stackage-curator-2 update &&
  stackage-curator-2 constraints &&
  stackage-curator-2 snapshotincomplete &&
  stackage-curator-2 snapshot &&
  stackage-curator-2 checksnapshot
