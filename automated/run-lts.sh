#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

git pull
LOG_FILE="logs/$1-build-$(date -u +%F+%T).log"
if [ -L $1-build.log ]; then
    mv -f $1-build.log $1-build-previous.log
fi
ln -sf $LOG_FILE lts-build.log
time script -c "./build.sh $*" $LOG_FILE
touch -h lts-build.log
date
