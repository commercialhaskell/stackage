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
apt-get install -y software-properties-common python-software-properties git

add-apt-repository ppa:hvr/ghc -y
add-apt-repository -y ppa:zoogie/sdl2-snapshots
add-apt-repository -y ppa:marutter/rrutter
add-apt-repository -y ppa:openstack-ubuntu-testing/icehouse

# Get Stack
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list

apt-get update
apt-get install -y \
    build-essential \
    ghc-7.10.3 \
    ghc-7.10.3-htmldocs \
    hscolour \
    sudo \
    curl \
    freeglut3-dev \
    git \
    libadns1-dev \
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
    libglib2.0-dev \
    libglu1-mesa-dev \
    libgmp3-dev \
    libgsasl7-dev \
    libgsl0-dev \
    libgtk-3-dev \
    libgtk2.0-dev \
    libhidapi-dev \
    libicu-dev \
    libjudy-dev \
    liblapack-dev \
    libleveldb-dev \
    liblzma-dev \
    libmagic-dev \
    libmagickcore-dev \
    libmagickwand-dev \
    libmysqlclient-dev \
    libncurses-dev \
    libnotify-dev \
    libopenal-dev \
    libpango1.0-dev \
    libpcap0.8-dev \
    libphash0-dev \
    libpq-dev \
    libsdl2-dev \
    libsnappy-dev \
    libsndfile1-dev \
    libsqlite3-dev \
    libssl-dev \
    libtagc0-dev \
    libtre-dev \
    libudev-dev \
    libusb-1.0-0-dev \
    libxau-dev \
    libxml2-dev \
    libxss-dev \
    libyaml-dev \
    libzip-dev \
    libzmq3-dev \
    llvm \
    m4 \
    nodejs \
    npm \
    r-base \
    r-base-dev \
    texlive-full \
    wget \
    zip \
    stack \
    zlib1g-dev

mkdir /tmp/nettle-build
(
cd /tmp/nettle-build
wget https://ftp.gnu.org/gnu/nettle/nettle-3.1.1.tar.gz
tar zxf nettle-3.1.1.tar.gz
cd nettle-3.1.1
./configure --prefix=/usr
make
make install

mkdir -p /usr/lib/x86_64-linux-gnu/
ln -sfv /usr/lib/libnettle.so.6.1 /usr/lib/x86_64-linux-gnu/libnettle.so.6
)
rm -rf /tmp/nettle-build

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
