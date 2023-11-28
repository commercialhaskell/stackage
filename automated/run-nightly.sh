#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    git pull
    ./build.sh nightly-$(date -u +%F)
    date

    sleep 30m
    echo
done
