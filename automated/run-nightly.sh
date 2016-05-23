#!/usr/bin/env bash

while true; do
    /opt/stackage-build/stackage/automated/build.sh nightly-$(date -u +%F)
    date
    echo

    echo "Running stackage-server-cron..."
    echo "('tail -f /home/ubuntu/stackage-server-cron.log' to watch)"
    /home/ubuntu/stackage-server-cron.sh >> /home/ubuntu/stackage-server-cron.log 2>&1
    echo "done."
    date

    sleep 30m
done
