#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    ./build.sh nightly-$(date -u +%F)

    echo "Running cron.sh (hiding verbose output)"
    ./cron.sh > cron.log 2>&1 # | grep -v '^Skipping'
    echo "done."
    date

    sleep 30m
done
