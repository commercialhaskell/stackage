#!/bin/sh

if [ "$#" = 0 ]; then
   echo "Usage: $0 GHCVER"
   exit 1
fi

set -e -x

# Run inside container env (`run-nightly.sh bash`)
# Usage: install-ghc-tarball.sh 9.12.4.20261102

cd ~/.stack/programs/x86_64-linux/

GHC_VERSION=$1

GHC_TARBALL=ghc-$GHC_VERSION-x86_64-ubuntu22_04-linux.tar.xz

wget https://downloads.haskell.org/~ghc/$GHC_VERSION/$GHC_TARBALL

tar xf $GHC_TARBALL

( cd ghc-${GHC_VERSION}-x86_64-unknown-linux
  ./configure --prefix=$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-$GHC_VERSION
  make install
)

touch ghc-tinfo6-$GHC_VERSION.installed
