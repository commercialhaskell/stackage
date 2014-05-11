#!/bin/bash -ex

cabal update
(cd patching/ && ./scripts/create-tarballs.sh)
cabal install
cabal install Cabal
./dist/build/stackage/stackage select --no-platform
./dist/build/stackage/stackage check
./dist/build/stackage/stackage build
./dist/build/stackage/stackage test
bash create-stackage-tarball.sh
