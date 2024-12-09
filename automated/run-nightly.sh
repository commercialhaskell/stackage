#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

case $1 in
        1) once=1 ;;
        *) cmd=$1
esac

while true; do
    git pull
    ./build.sh nightly-$(date -u +%F) $cmd
    ${cmd:+exit 0}
    date
    ${once:+exit 0}
    sleep 60m
    echo
done
