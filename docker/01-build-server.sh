#!/usr/bin/env bash

# This file contains setup needed for the build server to function at all.

set -exuo pipefail

mkdir -p /home/stackage

export LANG=C.UTF-8
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y curl unzip
