#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

git pull
LOG_FILE="logs/$1-build-$(date -u +%F+%T).log"
ln -sf $LOG_FILE lts-build.log
time script -q -c "./build.sh $*" $LOG_FILE
touch -h lts-build.log
date
