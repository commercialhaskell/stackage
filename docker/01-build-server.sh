#!/usr/bin/env bash

# This file contains setup needed for the build server to function at all.

set -exuo pipefail

mkdir -p /home/stackage

export LANG=C.UTF-8
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y curl unzip

# Install AWS CLI
mkdir -p /tmp/awscli
(
cd /tmp/awscli
# use 2.22.35 to workaround upload errors with 2.23:
# see https://github.com/haskellfoundation/hf-infrastructure/issues/11
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64-2.22.35.zip" -o "awscliv2.zip"
unzip -q awscliv2.zip
./aws/install --bin-dir /usr/bin
)
rm -rf /tmp/awscli
