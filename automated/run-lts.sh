#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

git pull
LOG_FILE="$1-build-$(date -u +%F_%T).log"
time script -q -c "./build.sh $*" $LOG_FILE
ln -sf $LOG_FILE lts-build-last.log
date
