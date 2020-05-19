#!/bin/bash

set -e

if [ $# != 1 ]; then
    echo "Usage: $0 [nightly|lts-xx]"
    exit 1
fi

SRCDIR=$(dirname $0)

pushd ~/stackage/automated/work/$1/unpack-dir/.stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-*

stack --resolver lts-14 script ${SRCDIR}/remove-old-stack-work-libs.hs
