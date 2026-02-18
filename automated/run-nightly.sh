#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

case $1 in
        1) once=1 ;;
        *) cmd=$1
esac

while true; do
    git pull
    LOG_FILE="nightly-build-$(date -u +%F_%T).log"
    if [ -L nightly-build.log ]; then
        mv -f nightly-build.log nightly-build-previous.log
    fi
    ln -sf $LOG_FILE nightly-build.log
    time script -c "./build.sh nightly-$(date -u +%F) $cmd" $LOG_FILE
    touch -h $LOG_FILE
    ${cmd:+exit 0}
    ${once:+date; exit 0}
    echo "$0: run completed at $(date)"
    echo
    sleep 90m
    echo
done
