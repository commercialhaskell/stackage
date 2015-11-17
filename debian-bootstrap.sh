#!/bin/bash -ex

# Work in progress: create a list of commands necessary to get Stackage
# up-and-running on a freshly installed Debian-based system (including Ubuntu).

# Quick start:
# wget -O - https://raw.github.com/fpco/stackage/master/debian-bootstrap.sh | bash -ex

# NOTE: Requires that GHC and Cabal are installed and on your PATH. For
# instructions, see:
#    http://www.stackage.org/install

add-apt-repository -y ppa:zoogie/sdl2-snapshots
add-apt-repository -y ppa:marutter/rrutter
add-apt-repository -y ppa:openstack-ubuntu-testing/icehouse

apt-get update
apt-get install -y \
    build-essential \
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
    zlib1g-dev

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
