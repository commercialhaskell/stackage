#!/usr/bin/env bash

set -eux

CRONDIR=$(pwd)/crondir
mkdir -p $CRONDIR
source aws.sh

echo "Running stackage-server-cron..."
echo "('tail -f $CRONDIR/stackage-server-cron.log' to watch)"
docker run --rm \
    -v $CRONDIR:/home/ubuntu \
    -w /home/ubuntu \
    fpco/stackage-server-prod:latest \
    bash -c "useradd $(whoami) -u $(id -u); sudo -u $(whoami) env HOME=/home/ubuntu AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY bash -c '/usr/local/bin/stackage-server-cron >> /home/ubuntu/stackage-server-cron.log 2>&1'"
