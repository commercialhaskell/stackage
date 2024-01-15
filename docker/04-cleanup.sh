#!/usr/bin/env bash

set -exuo pipefail

export LANG=C.UTF-8
export DEBIAN_FRONTEND=noninteractive

# Cleanup
apt-get clean
rm -rf /var/lib/apt/lists/*
