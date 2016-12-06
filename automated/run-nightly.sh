#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

while true; do
    ./build.sh nightly-$(date -u +%F)
    if ! pgrep cron.sh >/dev/null; then
        echo
        echo "Running cron.sh in the background"
        ./cron.sh > cron.log 2>&1 &
    fi
    echo
    date

    sleep 30m
done
