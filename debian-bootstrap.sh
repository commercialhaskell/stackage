#!/usr/bin/env bash

# Work in progress: create a list of commands necessary to get Stackage
# up-and-running on a freshly installed Debian-based system (including Ubuntu).

# Quick start:
# wget -O - https://raw.github.com/fpco/stackage/master/debian-bootstrap.sh | bash -ex

# NOTE: Requires that GHC and Cabal are installed and on your PATH. For
# instructions, see:
#    http://www.stackage.org/install

set -exu

mkdir /home/stackage -p
locale-gen en_US.UTF-8

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y software-properties-common

add-apt-repository ppa:hvr/ghc -y
add-apt-repository -y ppa:marutter/rrutter
# not sure what this was needed for
#add-apt-repository -y ppa:openstack-ubuntu-testing/icehouse

# Set the GHC version
GHCVER=8.0.1

# Get Stack
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu xenial main'|tee /etc/apt/sources.list.d/fpco.list

apt-get update
apt-get install -y \
    build-essential \
    ghc-$GHCVER \
    ghc-$GHCVER-htmldocs \
    hscolour \
    sudo \
    curl \
    freeglut3-dev \
    git \
    xclip \
    libadns1-dev \
    libasound2-dev \
    libblas-dev \
    libbz2-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libdevil-dev \
    libedit-dev \
    libedit2 \
    libfftw3-dev \
    libfreenect-dev \
    libgd2-xpm-dev \
    libgeoip-dev \
    libgirepository1.0-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libgmp3-dev \
    libgnutls-dev \
    libgsasl7-dev \
    libgsl0-dev \
    libgtk-3-dev \
    libgtk2.0-dev \
    libgtksourceview-3.0-dev \
    libhidapi-dev \
    libicu-dev \
    libjudy-dev \
    liblapack-dev \
    libleveldb-dev \
    liblzma-dev \
    libmagic-dev \
    libmagickcore-dev \
    libmagickwand-dev \
    libmarkdown2-dev \
    libmysqlclient-dev \
    libncurses-dev \
    libnotify-dev \
    libopenal-dev \
    libpango1.0-dev \
    libpcap0.8-dev \
    libpq-dev \
    libsdl2-dev \
    libsnappy-dev \
    libsndfile1-dev \
    libsqlite3-dev \
    libssl-dev \
    libsystemd-dev \
    libtagc0-dev \
    libtre-dev \
    libudev-dev \
    libusb-1.0-0-dev \
    libwebkitgtk-3.0-dev \
    libxau-dev \
    libxml2-dev \
    libxrandr-dev \
    libxss-dev \
    libyaml-dev \
    libzip-dev \
    libzmq3-dev \
    llvm \
    m4 \
    nettle-dev \
    nodejs \
    npm \
    r-base \
    r-base-dev \
    ruby-dev \
    wget \
    zip \
    stack \
    openjdk-8-jdk \
    zlib1g-dev

# Put documentation where we expect it
mv /opt/ghc/$GHCVER/share/doc/ghc-$GHCVER/ /opt/ghc/$GHCVER/share/doc/ghc

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
