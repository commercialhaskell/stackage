#!/usr/bin/env bash

while true; do
    /opt/stackage-build/stackage/automated/build.sh nightly-$(date -u +%F)
    date
    echo

    /home/ubuntu/stackage-server-cron.sh | grep -v Skipping | tee -a /home/ubuntu/stackage-server-cron.log
    date

    sleep 30m
done
