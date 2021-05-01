#!/usr/bin/env bash

set -eux

rm -rf bin
mkdir -p bin

docker build . --tag curator-exes

docker run --rm -v $(pwd)/bin:/output curator-exes cp /artifacts/* output
aws s3 cp curators-exes/* s3://download.fpcomplete.com/curator-exes/
