#!/usr/bin/env bash

set -eu +x

NIX_VERSION="2.0.4"
curl https://nixos.org/releases/nix/nix-$NIX_VERSION/install | sh

# no code to install stack2nix yet

stack2nix --cabal2nix-args="--no-hpack" unpack-dir > snapshot.nix

nix build -f snapshot-fixed.nix
