#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    ./build-next.sh nightly-$(date -u +%F)
    date

    #./new-stackage-format/convert.sh > /dev/null 2> /dev/null
    #date

    sleep 30m
    echo
done
