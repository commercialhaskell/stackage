#!/bin/bash -ex

cabal update
cabal install
./dist/build/stackage-nightly/stackage-nightly
