#!/bin/bash

set -e

if [ $# != 1 ]; then
    echo "Usage: $0 [nightly|lts-xx]"
    exit 1
fi

cd ~/stackage/automated/work/$1/unpack-dir/.stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-*
pwd

stack --resolver lts-14.27 script ~/stackage/etc/diskspace/remove-old-stack-work-libs.hs
