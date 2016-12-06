#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    ./build.sh nightly-$(date -u +%F)

    date
    echo -n "Running cron.sh (do not interrupt)... "
    ./cron.sh > cron.log 2>&1
    echo "done."
    echo
    date

    sleep 30m
done
