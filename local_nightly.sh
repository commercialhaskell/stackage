#!/usr/bin/env bash
cp build-constraints.yaml ./automated/work/nightly
cd automated
./build.sh nightly-$(date -u +%F)
