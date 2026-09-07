#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

if [ -z "$1" ];
   echo "Usage: $0 lts-X.Y"
   exit 1
fi

git pull
LOG_FILE="logs/$1-build-$(date -u +%F+%T).log"
if [ -L $1-build.log ]; then
    mv -f $1-build.log $1-build.log-prev
fi
ln -sf $LOG_FILE lts-build.log
time script -c "./build.sh $*" $LOG_FILE
touch -h lts-build.log
LANG=C date
