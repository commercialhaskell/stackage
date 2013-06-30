#!/bin/bash -ex

shopt -s nullglob

mkdir -p patches work

for f in work/*
do
    (
    cd $f
    PKG=$(basename $(pwd))
    cabal sdist
    cd ../..
    rm -rf tmp
    mkdir tmp
    cd tmp
    tar zxfv ../$f/dist/$PKG.tar.gz
    mv $PKG new
    cabal unpack $PKG
    mv $PKG orig
    diff -ru orig new > ../patches/$PKG.patch || true
    cd ..
    rm -rf tmp
    )
done
