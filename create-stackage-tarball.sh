#!/bin/bash -ex

tar czfv ghc-$(ghc --numeric-version)-$(date +%Y-%m-%d).stackage hackage desc patching/tarballs/
