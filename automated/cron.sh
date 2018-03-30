#!/usr/bin/env bash

set -eux

cd "$(dirname "${BASH_SOURCE[0]}")"

CRONDIR=$(pwd)/crondir
mkdir -p $CRONDIR
source aws.sh

IMAGE=fpco/stackage-server-prod:latest
docker pull $IMAGE

stack update

date
echo "Running stackage-server-cron..."
docker run --rm \
    -v $CRONDIR:/home/ubuntu \
    -v $HOME/.stack/indices:/home/ubuntu/.stack/indices:ro \
    --workdir /home/ubuntu \
    -p 17834:17834 \
    $IMAGE \
    bash -c "useradd $(whoami) -u $(id -u); sudo -u $(whoami) env HOME=/home/ubuntu AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY bash -c '/usr/local/bin/stackage-server-cron 2>&1 | tee -a /home/ubuntu/stackage-server-cron.log'"
