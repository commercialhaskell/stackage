#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

git pull
LOG_FILE="$1-build-$(date -u +%F_%T).log"
ln -sf $LOG_FILE lts-build.log
time script -q -c "./build.sh $*" $LOG_FILE
touch -h $LOG_FILE
date
