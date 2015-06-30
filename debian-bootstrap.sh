#!/bin/bash -ex

# Work in progress: create a list of commands necessary to get Stackage
# up-and-running on a freshly installed Debian-based system (includin Ubuntu).

# Quick start:
# wget -O - https://raw.github.com/fpco/stackage/master/debian-bootstrap.sh | bash -ex

# NOTE: Requires that GHC and Cabal are installed and on your PATH. For
# instructions, see:
#    http://www.stackage.org/install

add-apt-repository -y ppa:zoogie/sdl2-snapshots

apt-get update
apt-get install -y \
    build-essential \
    libncurses-dev \
    git \
    wget \
    m4 \
    texlive-full \
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
    libsqlite3-dev \
    libmysqlclient-dev \
    libpq-dev \
    libicu-dev \
    libssl-dev \
    libgsl0-dev \
    libblas-dev \
    liblapack-dev \
    libcurl4-openssl-dev \
    libfreenect-dev \
    libnotify-dev \
    libgd2-xpm-dev \
    libyaml-dev \
    liblzma-dev \
    libsdl2-dev \
    libxss-dev \
    libgtk-3-dev \
    libxml2-dev \
    libgsasl7-dev \
    libphash0-dev \
    libopenal-dev \
    libhidapi-dev \
    libzmq3-dev

mkdir /tmp/nettle-build
(
cd /tmp/nettle-build
wget https://ftp.gnu.org/gnu/nettle/nettle-2.7.1.tar.gz
tar zxf nettle-2.7.1.tar.gz
cd nettle-2.7.1
./configure --prefix=/usr
make
make install

mkdir -p /usr/lib/x86_64-linux-gnu/
ln -sfv /usr/lib/libnettle.so.4.7 /usr/lib/x86_64-linux-gnu/libnettle.so.4
)
rm -rf /tmp/nettle-build
