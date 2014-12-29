#!/bin/bash -ex

# Work in progress: create a list of commands necessary to get Stackage
# up-and-running on a freshly installed Debian-based system (includin Ubuntu).

# Quick start:
# wget -O - https://raw.github.com/fpco/stackage/master/debian-bootstrap.sh | bash -ex

# NOTE: Requires that GHC and Cabal are installed and on your PATH. For
# instructions, see:
#    http://www.stackage.org/install

sudo add-apt-repository -y ppa:chris-lea/zeromq
sudo add-apt-repository -y ppa:floe/libtisch
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    libncurses-dev \
    git \
    libgmp3c2 \
    libgmp3-dev \
    zlib1g-dev \
    libedit2 \
    libedit-dev \
    freeglut3-dev \
    libglu1-mesa-dev \
    libglib2.0-dev \
    libcairo2-dev \
    libpango1.0-dev \
    libgtk2.0-dev \
    zip \
    libdevil-dev \
    llvm \
    libbz2-dev \
    libjudy-dev \
    libmysqlclient-dev \
    libpq-dev \
    libicu-dev \
    libssl-dev \
    nettle-dev \
    libgsl0-dev \
    libblas-dev \
    liblapack-dev \
    libcurl4-openssl-dev \
    libfreenect-dev \
    libnotify-dev \
    libgd2-xpm-dev \
    libyaml-dev \
    liblzma-dev \
    libzmq3-dev
