#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

git pull
time script -q -c "./build.sh $*" $1-build-$(date -u +%F_%T).log
date
