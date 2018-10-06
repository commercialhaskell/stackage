#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    ./build.sh nightly-$(date -u +%F)
    date

    ./new-stackage-format/convert.sh
    date

    sleep 30m
    echo
done
