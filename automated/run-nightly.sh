#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    git pull
    ./build.sh nightly-$(date -u +%F) $1
    ${1:+exit 0}
    date

    sleep 49m
    echo
done
