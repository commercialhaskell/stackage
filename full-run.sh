#!/bin/bash -ex

runghc app/stackage.hs uploads
runghc app/stackage.hs select $*
runghc app/stackage.hs check
runghc app/stackage.hs build
runghc app/stackage.hs test
