#!/bin/bash -ex

mkdir -p work
cd work
rm -rf $1
cabal unpack $1
