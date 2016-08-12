#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    ./build.sh nightly-$(date -u +%F)
    date
    echo

    echo "Running cron.sh (hiding verbose output)"
    ./cron.sh | grep -v '^Skipping'
    echo "done."
    date

    sleep 30m
done
